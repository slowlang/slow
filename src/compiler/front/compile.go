package front

import (
	"context"
	"strconv"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"

	"github.com/slowlang/slow/src/compiler/ast"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	state struct {
		*ir.Func

		vars map[string]ir.Expr

		cond ir.Expr
		then *state
		els  *state
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

	s := newState(f, nil)

	for i, p := range fn.Args {
		e := ir.Arg(i)
		id := ir.Expr(len(f.Exprs))

		f.Exprs = append(f.Exprs, e)
		f.In[i].Expr = id

		s.vars[string(p.Name)] = id
	}

	for i := range fn.RetArgs {
		f.Out[i].Expr = -1
	}

	err = c.compileBlock(ctx, s, fn.Body)
	if err != nil {
		return err
	}

	p.Funcs = append(p.Funcs, f)

	return
}

func (c *Front) compileBlock(ctx context.Context, f *state, b *ast.Block) (err error) {
	for _, s := range b.Stmts {
		switch s := s.(type) {
		case ast.Assignment:
			id, err := c.compileRExpr(ctx, f, s.Rhs)
			if err != nil {
				return errors.Wrap(err, "assignment rhs")
			}

			v, ok := s.Lhs.(ast.Ident)
			if !ok {
				return errors.New("unsupported lexpr: %T", s.Lhs)
			}

			f.vars[string(v)] = id
		case *ast.IfStmt:
			condExpr, err := c.compileRExpr(ctx, f, s.Cond)
			if err != nil {
				return errors.Wrap(err, "if cond")
			}

			thenState := newState(f.Func, f)

			err = c.compileBlock(ctx, thenState, s.Then)
			if err != nil {
				return errors.Wrap(err, "if then")
			}

			var elseState *state

			if s.Else == nil {
				elseState = f
			} else {
				elseState = newState(f.Func, f)

				err = c.compileBlock(ctx, elseState, s.Else)
				if err != nil {
					return errors.Wrap(err, "if else")
				}
			}

			f = newStateMerge(f.Func, condExpr, thenState, elseState)
		case ast.Return:
			id, err := c.compileRExpr(ctx, f, s.Value)
			if err != nil {
				return errors.Wrap(err, "return")
			}

			tlog.Printw("return", "expr", id)

			f.Func.Out[0].Expr = id
		default:
			return errors.New("unsupported statement: %T", s)
		}

		if err != nil {
			return errors.Wrap(err, "%T", s)
		}
	}

	return nil
}

func (c *Front) compileRExpr(ctx context.Context, s *state, e ast.Expr) (id ir.Expr, err error) {
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

		id = s.alloc(ir.Word(x))
	case ast.BinOp:
		l, err := c.compileRExpr(ctx, s, e.Left)
		if err != nil {
			return -1, errors.Wrap(err, "op lhs")
		}

		r, err := c.compileRExpr(ctx, s, e.Right)
		if err != nil {
			return -1, errors.Wrap(err, "op rhs")
		}

		switch e.Op {
		case "+":
			id = s.alloc(ir.Add{
				Left:  l,
				Right: r,
			})
		case ">":
			id = s.alloc(ir.Cmp{
				Cond:  ir.Cond(e.Op),
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

func newState(f *ir.Func, par *state) *state {
	s := &state{
		Func: f,
		vars: make(map[string]ir.Expr),
	}

	if par != nil {
		s.cond = -1
		s.then = par
	}

	return s
}

func newStateMerge(f *ir.Func, cond ir.Expr, t, e *state) *state {
	s := newState(f, nil)

	s.cond = cond
	s.then = t
	s.els = e

	return s
}

func (s *state) alloc(x any) (id ir.Expr) {
	id = ir.Expr(len(s.Func.Exprs))

	s.Func.Exprs = append(s.Func.Exprs, x)

	return id
}

func (s *state) findVar(n string) (id ir.Expr) {
	id, ok := s.vars[n]
	if ok {
		return id
	}

	if s.then == nil {
		return -1
	}

	if s.els == nil {
		return s.then.findVar(n)
	}

	t := s.then.findVar(n)
	e := s.els.findVar(n)

	if t == e {
		return t
	}

	return s.alloc(ir.Phi{
		Cond: s.cond,
		Then: t,
		Else: e,
	})
}
