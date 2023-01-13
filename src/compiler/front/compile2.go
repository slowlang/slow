package front

import (
	"context"
	"fmt"
	"strconv"
	"unsafe"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/slowlang/slow/src/compiler/ast"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	global struct {
		exprs []any
	}

	Scope struct {
		*global
		Label ir.Expr

		vars map[string]ir.Expr
		phi  map[string]ir.Expr

		code []ir.Expr

		par []*Scope
	}
)

func (c *Front) Compile(ctx context.Context) (p *ir.Package, err error) {
	p = &ir.Package{}

	for _, f := range c.files {
		for _, fn := range f.parsed.Funcs {
			err = c.compileFunc(ctx, p, fn)
			if err != nil {
				return nil, errors.Wrap(err, "func %v", fn.Name)
			}
		}
	}

	return p, nil
}

func (c *Front) compileFunc(ctx context.Context, p *ir.Package, fn *ast.Func) (err error) {
	f := &ir.Func{
		Name: fn.Name,
		In:   make([]ir.Param, len(fn.Args)),
		Out:  make([]ir.Param, len(fn.RetArgs)),
	}

	s := newScope("prologue")

	for i, p := range fn.Args {
		a := ir.Arg(i)

		id := s.expr(a)

		f.In[i].Name = string(p.Name)
		f.In[i].Expr = id

		s.define(f.In[i].Name, id)
	}

	for i, p := range fn.RetArgs {
		f.Out[i].Name = string(p.Name)
		f.Out[i].Expr = -1

		if f.Out[i].Name == "" {
			f.Out[i].Name = fmt.Sprintf("ret_%d", i)
		}
	}

	end, err := c.compileBlock(ctx, s, fn.Body)
	if err != nil {
		return errors.Wrap(err, "body")
	}

	exit := newScope("epilogue", end)

	p.Funcs = append(p.Funcs, f)

	for i, p := range f.Out {
		id := exit.findVar(p.Name)

		f.Out[i].Expr = id
	}

	rename := make([]ir.Expr, len(s.exprs))

	add := func(ids ...ir.Expr) {
		for _, id := range ids {
			rename[id] = ir.Expr(len(f.Code))
			f.Code = append(f.Code, s.exprs[id])
		}
	}

	done := map[*Scope]struct{}{}
	var pr func(s *Scope)
	pr = func(s *Scope) {
		if _, ok := done[s]; ok {
			return
		}

		done[s] = struct{}{}

		par := make([]unsafe.Pointer, len(s.par))

		for i, p := range s.par {
			pr(p)
			par[i] = unsafe.Pointer(p)
		}

		tlog.Printw("scope", "label", s.Label, "phi", s.phi, "vars", s.vars, "p", unsafe.Pointer(s), "par", par)

		add(s.Label)

		for _, id := range s.phi {
			add(id)
		}

		add(s.code...)
	}

	pr(exit)

	for i, y := range f.Code {
		switch x := y.(type) {
		case ir.Label, ir.Arg, ir.Imm, ir.B:
		case ir.Cmp:
			y = ir.Cmp{L: rename[x.L], R: rename[x.R]}
		case ir.Add:
			y = ir.Add{L: rename[x.L], R: rename[x.R]}
		case ir.Sub:
			y = ir.Sub{L: rename[x.L], R: rename[x.R]}
		case ir.Mul:
			y = ir.Mul{L: rename[x.L], R: rename[x.R]}
		case ir.BCond:
			x.Expr = rename[x.Expr]
			y = x
		case ir.Phi:
			for j := range x {
				x[j] = rename[x[j]]
			}
		default:
			panic(x)
		}

		f.Code[i] = y
	}

	tlog.Printw("func ir", "name", f.Name, "in", f.In, "out", f.Out)

	for id, x := range f.Code {
		tlog.Printw("func code", "id", id, "typ", tlog.FormatNext("%T"), x, "val", x)
	}

	return nil
}

func (c *Front) compileBlock(ctx context.Context, s *Scope, b *ast.Block) (_ *Scope, err error) {
	for _, x := range b.Stmts {
		switch x := x.(type) {
		case ast.Return:
			id, err := c.compileExpr(ctx, s, x.Value)
			if err != nil {
				return nil, errors.Wrap(err, "return value")
			}

			_ = id

			// TODO
		case ast.Assignment:
			id, err := c.compileExpr(ctx, s, x.Rhs)
			if err != nil {
				return nil, errors.Wrap(err, "assignment rhs")
			}

			v, ok := x.Lhs.(ast.Ident)
			if !ok {
				return nil, errors.New("unsupported lexpr: %T", x.Lhs)
			}

			s.define(string(v), id)
		case *ast.IfStmt:
			s, err = c.compileIf(ctx, s, x)
			if err != nil {
				return s, errors.Wrap(err, "if")
			}
		case *ast.ForStmt:
			s, err = c.compileFor(ctx, s, x)
			if err != nil {
				return s, errors.Wrap(err, "for")
			}
		default:
			panic(x)
		}
	}

	return s, nil
}

func (c *Front) compileIf(ctx context.Context, s *Scope, x *ast.IfStmt) (_ *Scope, err error) {
	cond, condExpr, err := c.compileCond(ctx, s, x.Cond)
	if err != nil {
		return nil, errors.Wrap(err, "if cond")
	}

	thenLabel := ir.Label(fmt.Sprintf("then_%d", condExpr))
	endLabel := ir.Label(fmt.Sprintf("endif_%d", condExpr))

	s.addcode(ir.BCond{
		Expr:  condExpr,
		Cond:  cond,
		Label: thenLabel,
	})

	var elseLabel ir.Label

	if x.Else != nil {
		elseLabel = ir.Label(fmt.Sprintf("else_%d", condExpr))
	} else {
		elseLabel = endLabel
	}

	s.expr(ir.B{
		Label: elseLabel,
	})

	s.addcode(thenLabel)

	then := newScope(thenLabel, s)

	then, err = c.compileBlock(ctx, then, x.Then)
	if err != nil {
		return nil, errors.Wrap(err, "then")
	}

	els := s

	if x.Else != nil {
		then.expr(ir.B{
			Label: endLabel,
		})

		els = newScope(elseLabel, s)

		els, err = c.compileBlock(ctx, els, x.Else)
		if err != nil {
			return nil, errors.Wrap(err, "else")
		}
	}

	s = newScope(endLabel, then, els)

	return s, nil
}

func (c *Front) compileFor(ctx context.Context, s *Scope, x *ast.ForStmt) (_ *Scope, err error) {
	loopID := x.Pos
	condLabel := ir.Label(fmt.Sprintf("forcond_%d", loopID))

	s.addcode(ir.B{
		Label: condLabel,
	})

	loopCond := newScope(condLabel, s)

	cond, condExpr, err := c.compileCond(ctx, loopCond, x.Cond)
	if err != nil {
		return nil, errors.Wrap(err, "for cond")
	}

	bodyLabel := ir.Label(fmt.Sprintf("forbody_%d", loopID))

	loopCond.addcode(ir.BCond{
		Expr:  condExpr,
		Cond:  cond,
		Label: bodyLabel,
	})

	endLabel := ir.Label(fmt.Sprintf("endfor_%d", loopID))

	loopCond.addcode(ir.B{
		Label: endLabel,
	})

	loopBody := newScope(bodyLabel, loopCond)

	loopBody, err = c.compileBlock(ctx, loopBody, x.Body)
	if err != nil {
		return nil, errors.Wrap(err, "loop body")
	}

	loopBody.addcode(ir.B{
		Label: condLabel,
	})

	loopCond.addParent(loopBody)

	s = newScope(endLabel, loopCond)

	return s, nil
}

func (c *Front) compileCond(ctx context.Context, s *Scope, e ast.Expr) (cc ir.Cond, id ir.Expr, err error) {
	switch e := e.(type) {
	case ast.BinOp:
		switch e.Op {
		case "<", ">":
			cc = ir.Cond(e.Op)
		default:
			return "", -1, errors.New("unsupported op: %q", e.Op)
		}
	default:
		return "", -1, errors.New("unsupported expr: %T", e)
	}

	id, err = c.compileExpr(ctx, s, e)

	return
}

func (c *Front) compileExpr(ctx context.Context, s *Scope, e ast.Expr) (id ir.Expr, err error) {
	switch e := e.(type) {
	case ast.Ident:
		id = s.findVar(string(e))
		if id < 0 {
			return id, errors.New("undefined var: %s", e)
		}
	case ast.Number:
		x, err := strconv.ParseUint(string(e), 10, 64)
		if err != nil {
			return -1, errors.Wrap(err, "")
		}

		id = s.expr(ir.Imm(x))
	case ast.BinOp:
		l, err := c.compileExpr(ctx, s, e.Left)
		if err != nil {
			return -1, errors.Wrap(err, "op lhs")
		}

		r, err := c.compileExpr(ctx, s, e.Right)
		if err != nil {
			return -1, errors.Wrap(err, "op rhs")
		}

		var op any

		switch e.Op {
		case "+":
			op = ir.Add{
				L: l,
				R: r,
			}
		case "-":
			op = ir.Sub{
				L: l,
				R: r,
			}
		case "*":
			op = ir.Mul{
				L: l,
				R: r,
			}
		case "<", ">":
			op = ir.Cmp{
				L: l,
				R: r,
			}
		default:
			panic(e.Op)
		}

		id = s.expr(op)
	default:
		panic(e)
	}

	return
}

func newScope(l ir.Label, par ...*Scope) *Scope {
	s := &Scope{
		vars: map[string]ir.Expr{},
		phi:  map[string]ir.Expr{},

		par: par,
	}

	if len(par) == 0 {
		s.global = &global{}
	} else {
		s.global = par[0].global
	}

	s.Label = s.alloc(l)

	return s
}

func (s *Scope) addcode(x any) {
	id := s.alloc(x)

	s.code = append(s.code, id)
}

func (s *Scope) expr(x any) ir.Expr {
	id := s.alloc(x)

	s.code = append(s.code, id)

	return id
}

func (s *Scope) alloc(x any) ir.Expr {
	id := ir.Expr(len(s.exprs))
	s.exprs = append(s.exprs, x)

	//	tlog.Printw("scope alloc", "l", s.Label, "name", tlog.None, "id", id, "typ", tlog.FormatNext("%T"), x, "x", x, "from", loc.Callers(1, 3))

	return id
}

func (s *Scope) define(name string, id ir.Expr) {
	//	tlog.Printw("scope define", "l", s.Label, "name", name, "id", id, "from", loc.Callers(1, 3))
	s.vars[name] = id
}

func (s *Scope) findVar(name string) (id ir.Expr) {
	//	defer func() {
	//		tlog.Printw("scope findvar", "l", s.Label, "name", name, "id", id, "from", loc.Callers(1, 4))
	//	}()

	id, ok := s.vars[name]
	if ok {
		return id
	}

	id, ok = s.phi[name]
	if ok {
		return id
	}

	if len(s.par) == 0 {
		return -1
	}

	var phi ir.Phi

parents:
	for _, p := range s.par {
		id := p.findVar(name)
		if id == -1 {
			continue
		}

		for _, p := range phi {
			if id == p {
				continue parents
			}
		}

		phi = append(phi, id)
	}

	if len(phi) == 0 {
		return -1
	}

	//	if len(phi) == 1 {
	//		return phi[0]
	//	}

	id = s.alloc(phi)

	s.phi[name] = id

	return id
}

func (s *Scope) addParent(par *Scope) {
	s.par = append(s.par, par)

	for name, id := range s.phi {
		alt := par.findVar(name)
		if alt == -1 {
			continue
		}

		phi := s.exprs[id].(ir.Phi)

		phi = append(phi, alt)

		s.exprs[id] = phi
	}
}
