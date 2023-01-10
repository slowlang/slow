package front

import (
	"context"
	"fmt"
	"strconv"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"

	"github.com/slowlang/slow/src/compiler/ast"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	state struct {
		f     *ir.Func
		block int

		vars  map[string]ir.Expr
		phi   map[string]ir.Expr
		cache map[any]ir.Expr

		exit *state
		par  []*state
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

	s := newState(f)

	f.Entry = s.block

	for i, p := range fn.Args {
		a := ir.Arg(i)

		id := ir.Expr(len(f.Exprs))
		f.Exprs = append(f.Exprs, a)

		phi := ir.Phi{id}

		id2 := ir.Expr(len(f.Exprs))
		f.Exprs = append(f.Exprs, phi)

		f.In[i].Name = string(p.Name)
		f.In[i].Expr = id

		f.Blocks[s.block].Phi = append(f.Blocks[s.block].Phi, id2)

		s.define(f.In[i].Name, id2)
	}

	for i, p := range fn.RetArgs {
		f.Out[i].Name = string(p.Name)

		if f.Out[i].Name == "" {
			f.Out[i].Name = fmt.Sprintf("ret_%d_%p", i, fn)
		}
	}

	end, err := c.compileBlock(ctx, s, fn.Body)
	if err != nil {
		return errors.Wrap(err, "body")
	}

	s.exit.addParent(end)

	for i, p := range f.Out {
		id := s.exit.findVar(p.Name)
		if id == -1 {
			id = end.alloc(ir.Imm(0))
		}

		f.Out[i].Expr = id
	}

	p.Funcs = append(p.Funcs, f)

	var pr func(s *state)
	done := map[*state]struct{}{}

	pr = func(s *state) {
		if _, ok := done[s]; ok {
			return
		}

		done[s] = struct{}{}

		par := make([]int, len(s.par))

		for i, p := range s.par {
			pr(p)
			par[i] = p.block
		}

		tlog.Printw("block", "block", s.block, "vars", s.vars, "par", par)
	}

	pr(s.exit)

	return nil
}

func (c *Front) compileBlock(ctx context.Context, s *state, b *ast.Block) (_ *state, err error) {
	for _, x := range b.Stmts {
		switch x := x.(type) {
		case ast.Return:
			id, err := c.compileExpr(ctx, s, x.Value)
			if err != nil {
				return nil, errors.Wrap(err, "return value")
			}

			s.define(s.f.Out[0].Name, id)

			s.alloc(ir.B{
				Block: s.exit.block,
			})

			//	s.f.Blocks[s.block].Next = s.exit.block
			//	s.exit.par = append(s.exit.par, s)

			s.exit.addParent(s)
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
			cond, condExpr, err := c.compileCond(ctx, s, x.Cond)
			if err != nil {
				return nil, errors.Wrap(err, "if cond")
			}

			then := newState(s.f, s)

			s.alloc(ir.BCond{
				Expr:  condExpr,
				Cond:  cond,
				Block: then.block,
			})

			then, err = c.compileBlock(ctx, then, x.Then)
			if err != nil {
				return nil, errors.Wrap(err, "then")
			}

			els := s

			if x.Else != nil {
				els = newState(s.f, s)

				s.alloc(ir.B{
					Block: els.block,
				})

				els, err = c.compileBlock(ctx, els, x.Else)
				if err != nil {
					return nil, errors.Wrap(err, "else")
				}
			}

			next := newState(s.f, then, els)

			then.alloc(ir.B{
				Block: next.block,
			})

			els.alloc(ir.B{
				Block: next.block,
			})

			s = next

			/*
					cmp
					B.cond then
					B else
				then:
					// ...
					B next
				else:
					// ...
					B next
				next:

					cmp
					B.cond then
					B next
				then:
					// ...
					B next
				next:
			*/
		case *ast.ForStmt:
			loopCond := newState(s.f, s)
			loopCond.b().Loop++

			s.alloc(ir.B{Block: loopCond.block})

			cond, condExpr, err := c.compileCond(ctx, loopCond, x.Cond)
			if err != nil {
				return nil, errors.Wrap(err, "for cond")
			}

			loopBody := newState(s.f, loopCond)

			loopBody, err = c.compileBlock(ctx, loopBody, x.Body)
			if err != nil {
				return nil, errors.Wrap(err, "loop body")
			}

			loopBody.alloc(ir.B{
				Block: loopCond.block,
			})

			loopCond.addParent(loopBody)

			next := newState(s.f, loopCond)
			next.b().Loop = s.b().Loop

			loopCond.alloc(ir.BCond{
				Expr:  condExpr,
				Cond:  cond,
				Block: loopBody.block,
			})

			loopCond.alloc(ir.B{Block: next.block})

			s = next
		default:
			return nil, errors.New("unsupported statement: %T", x)
		}
	}

	return s, nil
}

func (c *Front) compileCond(ctx context.Context, s *state, e ast.Expr) (cc ir.Cond, id ir.Expr, err error) {
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

func (c *Front) compileExpr(ctx context.Context, s *state, e ast.Expr) (id ir.Expr, err error) {
	switch e := e.(type) {
	case ast.Ident:
		id = s.findVar(string(e))
		if id < 0 {
			return id, errors.New("undefined var: %s", e)
		}

		return id, nil
	case ast.Number:
		x, err := strconv.ParseUint(string(e), 10, 64)
		if err != nil {
			return -1, errors.Wrap(err, "")
		}

		id = s.alloc(ir.Imm(x))
	case ast.BinOp:
		l, err := c.compileExpr(ctx, s, e.Left)
		if err != nil {
			return -1, errors.Wrap(err, "op lhs")
		}

		r, err := c.compileExpr(ctx, s, e.Right)
		if err != nil {
			return -1, errors.Wrap(err, "op rhs")
		}

		switch e.Op {
		case "+":
			id = s.alloc(ir.Add{
				L: l,
				R: r,
			})
		case "<", ">":
			id = s.alloc(ir.Cmp{
				L: l,
				R: r,
			})
		default:
			return -1, errors.New("unsupported op: %q", e.Op)
		}
	default:
		return -1, errors.New("unsupported expr: %T", e)
	}

	return
}

func newState(f *ir.Func, par ...*state) *state {
	s := &state{
		f:     f,
		block: len(f.Blocks),
		vars:  make(map[string]ir.Expr),
		phi:   make(map[string]ir.Expr),
		par:   par,
	}

	f.Blocks = append(f.Blocks, ir.Block{})

	tlog.Printw("new block", "par", len(par), "from", loc.Caller(1))

	if len(par) == 0 {
		f.Blocks = append(f.Blocks, ir.Block{})

		s.cache = map[any]ir.Expr{}
		s.exit = &state{
			f:     f,
			block: 0,
			vars:  make(map[string]ir.Expr),
			phi:   make(map[string]ir.Expr),
		}

		s.block = 1
	} else {
		s.cache = par[0].cache
		s.exit = par[0].exit

		s.b().Loop = par[0].b().Loop
	}

	return s
}

func (s *state) findVar(n string) (id ir.Expr) {
	id, ok := s.vars[n]
	if ok {
		return id
	}

	id, ok = s.phi[n]
	if ok {
		return id
	}

	if len(s.par) == 0 {
		return -1
	}

	var phi ir.Phi

parents:
	for _, p := range s.par {
		id := p.findVar(n)
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

	id = ir.Expr(len(s.f.Exprs))
	s.f.Exprs = append(s.f.Exprs, phi)
	s.f.Blocks[s.block].Phi = append(s.f.Blocks[s.block].Phi, id)

	//	s.vars[n] = id
	s.phi[n] = id

	return id
}

func (s *state) alloc(x any) (id ir.Expr) {
	//	if id, ok := s.cache[x]; ok {
	//		return id
	//	}

	id = ir.Expr(len(s.f.Exprs))
	s.cache[x] = id

	s.f.Exprs = append(s.f.Exprs, x)
	s.f.Blocks[s.block].Code = append(s.f.Blocks[s.block].Code, id)

	return id
}

func (s *state) define(name string, id ir.Expr) {
	s.vars[name] = id
}

func (s *state) addParent(par *state) {
	tlog.Printw("add parent", "s.vars", s.vars, "par.vars", par.vars)

vars:
	for name, id := range s.phi {
		p := s.f.Exprs[id].(ir.Phi)

		x := par.findVar(name)
		//	if p, ok := s.f.Exprs[x].(ir.Phi); ok && len(p) == 1 {
		//		x = p[0]
		//	}

		tlog.Printw("fix phi", "var", name, "phi", p, "newid", x)

		if x == id {
			continue
		}

		for _, p := range p {
			if p == x {
				continue vars
			}
		}

		s.f.Exprs[id] = append(p, x)
	}

	for _, x := range s.par {
		if par == x {
			return
		}
	}

	s.par = append(s.par, par)
}

func (s *state) b() *ir.Block {
	return &s.f.Blocks[s.block]
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

/*
func (c *Front) compileFunc(ctx context.Context, p *ir.Package, fn *ast.Func) (err error) {
	f := &ir.Func{
		Name: fn.Name,
		In:   make([]ir.Param, len(fn.Args)),
		Out:  make([]ir.Param, len(fn.RetArgs)),
	}

	exit := newState(f)

	s := newState(f)
	s.exit = exit

	for i, p := range fn.Args {
		e := ir.Arg(i)
		id := ir.Expr(len(f.Code))

		f.Code = append(f.Code, e)
		f.In[i].Name = string(p.Name)
		f.In[i].Expr = id

		s.vars[string(p.Name)] = id
		if b := &s.f.Blocks[s.block]; true {
			b.In = append(b.In, id)
		}
	}

	for i, p := range fn.RetArgs {
		f.Out[i].Name = string(p.Name)
		f.Out[i].Expr = -1
	}

	err = c.compileBlock(ctx, s, fn.Body)
	if err != nil {
		return err
	}

	for i, r := range fn.RetArgs {
		id := exit.findVar(string(r.Name), nil)

		f.Out[i].Expr = id

		exit.markVar(id, nil)
	}

	p.Funcs = append(p.Funcs, f)

	return
}

func (c *Front) compileBlock(ctx context.Context, f *state, b *ast.Block) (err error) {
	for _, s := range b.Stmts {
		switch s := s.(type) {
		case ast.Return:
			id, err := c.compileExpr(ctx, f, s.Value)
			if err != nil {
				return errors.Wrap(err, "return")
			}

			f.vars[f.f.Out[0].Name] = id
			f.exit.par = append(f.exit.par, f)
			//	f.markVar(id, f.exit)

			if b := f.bref(); true {
				id := f.alloc(ir.Branch{
					Block: f.exit.block,
				})

				b.Code = append(b.Code, id)
			}
		case ast.Assignment:
			id, err := c.compileExpr(ctx, f, s.Rhs)
			if err != nil {
				return errors.Wrap(err, "assignment rhs")
			}

			v, ok := s.Lhs.(ast.Ident)
			if !ok {
				return errors.New("unsupported lexpr: %T", s.Lhs)
			}

			f.vars[string(v)] = id
		case *ast.IfStmt:
			cond, condExpr, err := c.compileCond(ctx, f, s.Cond)
			if err != nil {
				return errors.Wrap(err, "if cond")
			}

			thenState := newState(f.f, f)

			err = c.compileBlock(ctx, thenState, s.Then)
			if err != nil {
				return errors.Wrap(err, "if then")
			}

			var elseState *state = f

			if s.Else != nil {
				elseState = newState(f.f, f)

				err = c.compileBlock(ctx, elseState, s.Else)
				if err != nil {
					return errors.Wrap(err, "if else")
				}
			}

			if b := f.bref(); true {
				id := f.alloc(ir.BranchIf{
					Cond:  cond,
					Expr:  condExpr,
					Block: thenState.block,
				})

				b.Code = append(b.Code, id)

				id = f.alloc(ir.Branch{
					Block: elseState.block,
				})

				b.Code = append(b.Code, id)

				//	b.Ops = append(b.Ops, ir.BranchIf{Cond: cond, Expr: condExpr, Block: thenState.block})
				//	b.Ops = append(b.Ops, ir.Branch{Block: elseState.block})
				//	b.Next = elseState.block
			}

			next := newState(f.f, thenState, elseState)

			tlog.Printw("branch", "base", f.block, "then", thenState.block, "else", elseState.block, "next", next.block)

			if b := thenState.bref(); true {
				id := f.alloc(ir.Branch{
					Block: next.block,
				})

				b.Code = append(b.Code, id)
				//	b.Ops = append(b.Ops, ir.Branch{Block: next.block})
				//	b.Next = next.block
			}
			if b := elseState.bref(); elseState != f {
				id := f.alloc(ir.Branch{
					Block: next.block,
				})

				b.Code = append(b.Code, id)
				//	b.Ops = append(b.Ops, ir.Branch{Block: next.block})
				//	b.Next = next.block
			}

			f = next
		default:
			return errors.New("unsupported statement: %T", s)
		}

		if err != nil {
			return errors.Wrap(err, "%T", s)
		}
	}

	return nil
}

func (c *Front) compileCond(ctx context.Context, s *state, e ast.Expr) (cc ir.Cond, id ir.Expr, err error) {
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

func (c *Front) compileExpr(ctx context.Context, s *state, e ast.Expr) (id ir.Expr, err error) {
	switch e := e.(type) {
	case ast.Ident:
		id = s.findVar(string(e), nil)
		if id < 0 {
			return id, errors.New("undefined var: %s", e)
		}

		return id, nil
	case ast.Number:
		x, err := strconv.ParseUint(string(e), 10, 64)
		if err != nil {
			return -1, errors.Wrap(err, "")
		}

		id = s.alloc(ir.Word(x))
	case ast.BinOp:
		l, err := c.compileExpr(ctx, s, e.Left)
		if err != nil {
			return -1, errors.Wrap(err, "op lhs")
		}

		r, err := c.compileExpr(ctx, s, e.Right)
		if err != nil {
			return -1, errors.Wrap(err, "op rhs")
		}

		switch e.Op {
		case "+":
			id = s.alloc(ir.Add{
				Left:  l,
				Right: r,
			})
		case "<", ">":
			id = s.alloc(ir.Cmp{
				Left:  l,
				Right: r,
			})
		default:
			return -1, errors.New("unsupported op: %q", e.Op)
		}
	default:
		return -1, errors.New("unsupported expr: %T", e)
	}

	return
}

func newState(f *ir.Func, par ...*state) *state {
	s := &state{
		f:    f,
		vars: make(map[string]ir.Expr),
		par:  par,
	}

	s.block = len(f.Blocks)
	f.Blocks = append(f.Blocks, ir.Block{})

	if len(par) == 0 {
		s.cache = make(map[any]ir.Expr)
	} else {
		s.cache = par[0].cache
		s.exit = par[0].exit
	}

	return s
}

func (s *state) alloc(x any) (id ir.Expr) {
	if id, ok := s.cache[x]; ok {
		return id
	}

	id = ir.Expr(len(s.f.Code))
	s.cache[x] = id

	s.f.Code = append(s.f.Exprs, x)

	return id
}

func (s *state) findVar(n string, from *state) (id ir.Expr) {
	if from != nil {
		defer func() { s.markVar(id, from) }()
	}

	id, ok := s.vars[n]
	if ok {
		return id
	}

	if len(s.par) == 0 {
		return -1
	}

	id = s.par[0].findVar(n, s)

	var phi ir.Phi

parents:
	for _, p := range s.par[1:] {
		alt := p.findVar(n, s)
		if alt == id {
			continue
		}
		for _, p := range phi {
			if alt == p {
				continue parents
			}
		}

		if len(phi) == 0 {
			phi = append(phi, id)
		}

		phi = append(phi, alt)
	}

	if len(phi) == 0 {
		return id
	}

	id = ir.Expr(len(s.f.Code))
	s.f.Code = append(s.f.Code, phi)

	s.vars[n] = id

	return id
}

func (s *state) markVar(id ir.Expr, from *state) {
	if b := s.bref(); !exprIn(id, b.Out) {
		b.Out = append(b.Out, id)
	}

	if from == nil {
		return
	}

	if b := from.bref(); !exprIn(id, b.In) {
		b.In = append(b.In, id)
	}
}

func (s *state) bref() *ir.Block {
	return &s.f.Blocks[s.block]
}
*/

func exprIn(e ir.Expr, in []ir.Expr) bool {
	for _, x := range in {
		if e == x {
			return true
		}
	}

	return false
}
