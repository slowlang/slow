package front

import (
	"context"
	"fmt"
	"go/ast"
	"go/token"
	"strconv"
	"unsafe"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"

	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	global struct {
		*ir.Package
		exit ir.Label

		ret []ir.Phi

		ir.Label
	}

	Scope struct {
		*global
		Label ir.Expr

		vars map[string]ir.Expr
		phi  map[string]ir.Expr

		code []ir.Expr

		par []*Scope

		deadend bool
	}
)

func (c *Front) Compile(ctx context.Context) (p *ir.Package, err error) {
	p = &ir.Package{}

	for _, f := range c.files {
		//	tlog.Printw("file", "f", f)

		for _, d := range f.Decls {
			err = c.compileDecl(ctx, p, d)
			if err != nil {
				return nil, errors.Wrap(err, "decl %T", d)
			}
		}
	}

	return p, nil
}

func (c *Front) compileDecl(ctx context.Context, p *ir.Package, d ast.Decl) (err error) {
	switch d := d.(type) {
	case *ast.FuncDecl:
		return c.compileFunc(ctx, p, d)
	default:
		panic(d)
	}
}

func (c *Front) compileFunc(ctx context.Context, p *ir.Package, fn *ast.FuncDecl) (err error) {
	f := &ir.Func{
		Name: fn.Name.Name,
	}

	tlog.Printw("compile func", "name", f.Name)

	p.Funcs = append(p.Funcs, f)

	s := newScope(-1)
	s.Package = p

	exprStart := len(p.Exprs)

	for _, p := range fn.Type.Params.List {
		for _, name := range p.Names {
			a := ir.Arg(len(f.In))
			id := s.expr(a)
			p := ir.Param{
				Name: name.Name,
				Expr: id,
			}

			s.define(p.Name, id)

			f.In = append(f.In, p)
		}
	}

	for _, p := range fn.Type.Results.List {
		if len(p.Names) == 0 {
			p := ir.Param{
				Expr: -1,
			}

			f.Out = append(f.Out, p)

			continue
		}

		for _, name := range p.Names {
			p := ir.Param{
				Name: name.Name,
				Expr: -1,
			}

			f.Out = append(f.Out, p)
		}
	}

	s.global.ret = make([]ir.Phi, len(f.Out))

	end, err := c.compileBlock(ctx, s, fn.Body)
	if err != nil {
		return errors.Wrap(err, "body")
	}

	if !end.deadend {
		end.addcode(ir.B{Label: s.exit})
	}

	exit := newScope(s.exit, end)

	for i := range f.Out {
		id := exit.alloc(s.global.ret[i])

		f.Out[i].Expr = id

		tlog.Printw("return", "i", i, "id", id)
	}

	for i, x := range s.global.Package.Exprs[exprStart:] {
		id := ir.Expr(exprStart + i)
		tlog.Printw("expr", "id", id, "typ", tlog.FormatNext("%T"), x, "val", x)
	}

	add := func(ids ...ir.Expr) {
		f.Code = append(f.Code, ids...)
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

		var lab ir.Label = -1

		if s.Label >= 0 {
			lab = p.Exprs[s.Label].(ir.Label)
		}

		var next ir.Label = -1
		var bcond []ir.Label

		for _, id := range s.code {
			switch x := p.Exprs[id].(type) {
			case ir.BCond:
				bcond = append(bcond, x.Label)
			case ir.B:
				next = x.Label
			}
		}
		if l := len(s.code); l != 0 {
			if b, ok := p.Exprs[s.code[l-1]].(ir.B); ok {
				next = b.Label
			}
		}

		tlog.Printw("scope", "label", lab, "next", next, "bcond", bcond, "phi", s.phi, "vars", s.vars, "p", unsafe.Pointer(s), "par", par)

		if s.Label >= 0 {
			add(s.Label)
		}

		for _, id := range s.phi {
			add(id)
		}

		if s == exit {
			for _, p := range f.Out {
				add(p.Expr)
			}
		}

		add(s.code...)
	}

	pr(exit)

	if tlog.If("dump") {
		tlog.Printw("func ir", "name", f.Name, "in", f.In, "out", f.Out)

		for _, id := range f.Code {
			x := p.Exprs[id]

			tlog.Printw("func code", "id", id, "typ", tlog.FormatNext("%T"), x, "val", x)
		}
	}

	return nil
}

func (c *Front) compileBlock(ctx context.Context, s *Scope, b *ast.BlockStmt) (_ *Scope, err error) {
	for _, x := range b.List {
		switch x := x.(type) {
		case *ast.ReturnStmt:
			ids := make([]ir.Expr, len(x.Results))

			for i, e := range x.Results {
				ids[i], err = c.compileExpr(ctx, s, e)
				if err != nil {
					return nil, errors.Wrap(err, "return value")
				}
			}

			s.ret(ids...)

			s.addcode(ir.B{
				Label: s.exit,
			})

			return s, nil
		case *ast.AssignStmt:
			if len(x.Lhs) != len(x.Rhs) {
				panic("bad assign")
			}

			ids := make([]ir.Expr, len(x.Rhs))

			for i, e := range x.Rhs {
				ids[i], err = c.compileExpr(ctx, s, e)
				if err != nil {
					return nil, errors.Wrap(err, "assignment rhs")
				}
			}

			for i, e := range x.Lhs {
				v, ok := e.(*ast.Ident)
				if !ok {
					return nil, errors.New("unsupported lexpr: %T", x.Lhs)
				}

				s.define(v.Name, ids[i])
			}
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

	if x.Else == nil {
		thenLabel := s.label()
		endLabel := s.label()

		s.addcode(ir.BCond{
			Expr:  condExpr,
			Cond:  revCond(cond),
			Label: endLabel,
		})

		s.addcode(ir.B{
			Label: thenLabel,
		})

		then := newScope(thenLabel, s)

		then, err = c.compileBlock(ctx, then, x.Body)
		if err != nil {
			return nil, errors.Wrap(err, "then")
		}

		if !then.deadend {
			then.addcode(ir.B{
				Label: endLabel,
			})
		}

		s = newScope(endLabel, s, then)

		return s, nil
	}

	thenLabel := s.label()
	endLabel := s.label()

	s.addcode(ir.BCond{
		Expr:  condExpr,
		Cond:  cond,
		Label: thenLabel,
	})

	var elseLabel ir.Label

	if x.Else != nil {
		elseLabel = s.label()
	} else {
		elseLabel = endLabel
	}

	s.addcode(ir.B{
		Label: elseLabel,
	})

	then := newScope(thenLabel, s)

	then, err = c.compileBlock(ctx, then, x.Body)
	if err != nil {
		return nil, errors.Wrap(err, "then")
	}

	els := s

	if x.Else != nil {
		then.addcode(ir.B{
			Label: endLabel,
		})

		els = newScope(elseLabel, s)

		els, err = c.compileBlock(ctx, els, x.Else.(*ast.BlockStmt))
		if err != nil {
			return nil, errors.Wrap(err, "else")
		}
	}

	s = newScope(endLabel, then, els)

	return s, nil
}

func (c *Front) compileFor(ctx context.Context, s *Scope, x *ast.ForStmt) (_ *Scope, err error) {
	if x.Cond == nil {
		bodyLabel := s.label()

		s.addcode(ir.B{Label: bodyLabel})

		loopBodyStart := newScope(bodyLabel, s)

		loopBody, err := c.compileBlock(ctx, loopBodyStart, x.Body)
		if err != nil {
			return nil, errors.Wrap(err, "loop body")
		}

		loopBodyStart.addParent(loopBody)

		loopBody.addcode(ir.B{
			Label: bodyLabel,
		})

		s = loopBody

		s.deadend = true

		return s, nil
	}

	condLabel := s.label()

	s.addcode(ir.B{
		Label: condLabel,
	})

	loopCond := newScope(condLabel, s)

	cond, condExpr, err := c.compileCond(ctx, loopCond, x.Cond)
	if err != nil {
		return nil, errors.Wrap(err, "for cond")
	}

	bodyLabel := s.label()

	loopCond.addcode(ir.BCond{
		Expr:  condExpr,
		Cond:  cond,
		Label: bodyLabel,
	})

	endLabel := s.label()

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
	case *ast.BinaryExpr:
		switch op := e.Op.String(); op {
		case "<", ">", "==", "!=", "<=", ">=":
			cc = ir.Cond(op)
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
	case *ast.Ident:
		id = s.findVar(e.Name, nil)
		if id < 0 {
			return id, errors.New("undefined var: %s", e)
		}
	case *ast.BasicLit:
		switch e.Kind {
		case token.INT:
			x, err := strconv.ParseUint(e.Value, 10, 64)
			if err != nil {
				return -1, errors.Wrap(err, "")
			}

			id = s.expr(ir.Imm(x))
		default:
			panic(e.Kind)
		}
	case *ast.BinaryExpr:
		l, err := c.compileExpr(ctx, s, e.X)
		if err != nil {
			return -1, errors.Wrap(err, "op lhs")
		}

		r, err := c.compileExpr(ctx, s, e.Y)
		if err != nil {
			return -1, errors.Wrap(err, "op rhs")
		}

		var op any

		switch e.Op.String() {
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
		case "<", ">", "<=", ">=", "==", "!=":
			op = ir.Cmp{
				L: l,
				R: r,
			}
		default:
			panic(e.Op)
		}

		id = s.expr(op)
	case *ast.CallExpr:
		n := e.Fun.(*ast.Ident)

		x := ir.Call{
			Func: n.Name,
			In:   make([]ir.Expr, len(e.Args)),
		}

		for i, a := range e.Args {
			x.In[i], err = c.compileExpr(ctx, s, a)
			if err != nil {
				return -1, errors.Wrap(err, "op lhs")
			}
		}

		id = s.expr(x)
	default:
		panic(fmt.Sprintf("%T: %[1]v", e))
	}

	return
}

func newScope(l ir.Label, par ...*Scope) *Scope {
	s := &Scope{
		Label: -1,

		vars: map[string]ir.Expr{},
		phi:  map[string]ir.Expr{},
	}

	tlog.Printw("new scope", "label", l, "par", len(par), "from", loc.Callers(1, 4))

	if len(par) == 0 {
		s.global = &global{}
		s.exit = s.label()
	} else {
		s.global = par[0].global
	}

	if l >= 0 {
		s.Label = s.alloc(l)
	}

	for _, par := range par {
		s.addParent(par)
	}

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
	id := ir.Expr(len(s.Exprs))
	s.Exprs = append(s.Exprs, x)

	//	tlog.Printw("scope alloc", "l", s.Label, "name", tlog.None, "id", id, "typ", tlog.FormatNext("%T"), x, "x", x, "from", loc.Callers(1, 3))

	return id
}

func (s *Scope) define(name string, id ir.Expr) {
	//	tlog.Printw("scope define", "l", s.Label, "name", name, "id", id, "from", loc.Callers(1, 3))
	s.vars[name] = id
}

func (s *Scope) findVar(name string, stop map[*Scope]struct{}) (id ir.Expr) {
	//	defer func() {
	//		tlog.Printw("scope findvar", "l", s.Label, "name", name, "id", id, "from", loc.Callers(1, 4))
	//	}()

	if _, ok := stop[s]; ok {
		return -1
	}

	if stop == nil {
		stop = map[*Scope]struct{}{}
	}

	stop[s] = struct{}{}

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
		if p.deadend {
			continue
		}

		id := p.findVar(name, stop)
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

	id = s.alloc(phi)

	s.phi[name] = id

	tlog.Printw("new phi", "id", id, "name", name, "label", s.Label, "from", loc.Callers(1, 4))

	return id
}

func (s *Scope) addParent(par *Scope) {
	s.par = append(s.par, par)

	for name, id := range s.phi {
		alt := par.findVar(name, nil)
		if alt == -1 {
			continue
		}

		phi := s.Exprs[id].(ir.Phi)

		phi = append(phi, alt)

		s.Exprs[id] = phi
	}
}

func (s *Scope) ret(ids ...ir.Expr) {
	if len(ids) != len(s.global.ret) {
		panic(fmt.Sprintf("return %d  expected %d", len(ids), len(s.global.ret)))
	}

	s.deadend = true

out:
	for i, id := range ids {
		for _, x := range s.global.ret[i] {
			if x == id {
				continue out
			}
		}

		s.global.ret[i] = append(s.global.ret[i], id)
	}
}

func (s *global) label() (l ir.Label) {
	l = s.Label
	s.Label++

	return l
}

func revCond(c ir.Cond) ir.Cond {
	switch c {
	case "<":
		return ">="
	case ">":
		return "<="
	case "<=":
		return ">"
	case ">=":
		return "<"
	case "==":
		return "!="
	case "!=":
		return "=="
	default:
		panic(string(c))
	}
}
