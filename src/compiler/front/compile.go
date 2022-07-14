//go:build ignore

package front

import (
	"context"
	"strconv"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/nikandfor/tlog/wire"

	"github.com/slowlang/slow/src/compiler/ast"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	CompState struct {
	}

	scope interface {
		new(name string) ir.Var
		get(name string) ir.Var
		put(name string, e ir.Expr) ir.Var

		expr(ir.Var) ir.Expr
	}

	block struct {
		prev scope

		vars  map[string]int
		graph map[ir.Var]ir.Expr
	}

	ifstmt struct {
		cond ir.Expr
		then scope
		alt  scope

		front *block
	}
)

func (s *State) Compile(ctx context.Context) error {
	ss := &CompState{}

	f := s.files[0].parsed

	for _, f := range f.Funcs {
		ff, err := ss.compileFunc(ctx, f)
		if err != nil {
			return errors.Wrap(err, "%v", f.Name)
		}

		_ = ff
		tlog.SpanFromContext(ctx).Printw("func", "ir", ff)
	}

	return nil
}

func (s *CompState) compileFunc(ctx context.Context, f *ast.Func) (ff *ir.Func, err error) {
	ff = &ir.Func{
		Name: f.Name,
	}

	b := newBlock(nil)

	for i, a := range f.Args {
		arg := ir.Arg{Num: i}
		v := b.put(a.Name, arg)

		ff.In = append(ff.In, arg)

		tlog.SpanFromContext(ctx).Printw("arg", "var", v, "arg", arg)
	}

	b, err = s.calcBlock(ctx, f, f.Body, b)
	if err != nil {
		return nil, errors.Wrap(err, "block")
	}

	for _, a := range f.RetArgs {
		v := b.get(a.Name)

		err = s.compileExpr(ctx, b, v)
		if err != nil {
			return nil, errors.Wrap(err, "compile")
		}
	}

	return ff, nil

}

func (s *CompState) compileExpr(ctx context.Context, b scope, e ir.Expr) (err error) {
	switch e := e.(type) {
	case ir.Var:
		ee := b.expr(e)

		err = s.compileExpr(ctx, b, ee)
		if err != nil {
			return errors.Wrap(err, "expr")
		}
	case ir.Phi:
		err = s.compileExpr(ctx, b, e.Cond)
		if err != nil {
			return errors.Wrap(err, "cond")
		}

	default:
		return errors.New("unsupported: %T", e)
	}

	return nil
}

func (s *CompState) calcBlock(ctx context.Context, f *ast.Func, bl *ast.Block, prev scope) (b *block, err error) {
	b = newBlock(prev)

loop:
	for _, x := range bl.Stmts {
		switch x := x.(type) {
		case ast.Assignment:
			r, err := s.calcRhsExpr(ctx, b, x.Rhs)
			if err != nil {
				return nil, errors.Wrap(err, "assignment rhs")
			}

			l, err := s.calcLhsExpr(ctx, b, x.Lhs)
			if err != nil {
				return nil, errors.Wrap(err, "assignment lhs")
			}

			tlog.SpanFromContext(ctx).Printw("assign", "b", tlog.FormatNext("%p"), b, "lhs", l, "rhs", r)

			b.graph[l.(ir.Var)] = r
		case ast.Return:
			e, err := s.calcRhsExpr(ctx, b, x.Value)
			if err != nil {
				return nil, errors.Wrap(err, "return")
			}

			v := b.put(f.RetArgs[0].Name, e)

			tlog.SpanFromContext(ctx).Printw("return", "var", v, "rhs", e)

			break loop
		case *ast.IfStmt:
			cond, err := s.calcRhsExpr(ctx, b, x.Cond)
			if err != nil {
				return nil, errors.Wrap(err, "if cond")
			}

			then, err := s.calcBlock(ctx, f, x.Then, b)
			if err != nil {
				return nil, errors.Wrap(err, "if then")
			}

			ifst := &ifstmt{cond: cond, then: then, alt: b}
			b = newBlock(ifst)

			tlog.SpanFromContext(ctx).Printw("if stmt", "cond", cond, "then", then)
		default:
			return nil, errors.New("unsupported type: %T", x)
		}
	}

	return b, nil
}

func (s *CompState) calcLhsExpr(ctx context.Context, b scope, e ast.Expr) (x ir.Expr, err error) {
	switch e := e.(type) {
	case ast.Ident:
		return b.new(string(e)), nil
	default:
		return nil, errors.New("unsupported type: %T", e)
	}
}

func (s *CompState) calcRhsExpr(ctx context.Context, b scope, e ast.Expr) (x ir.Expr, err error) {
	switch e := e.(type) {
	case ast.Ident:
		return b.get(string(e)), nil
	case ast.Number:
		v, err := strconv.ParseInt(string(e), 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "parse num")
		}

		return ir.Word{Value: v}, nil
	case ast.BinOp:
		l, err := s.calcRhsExpr(ctx, b, e.Left)
		if err != nil {
			return nil, errors.Wrap(err, "assignment lhs")
		}

		r, err := s.calcRhsExpr(ctx, b, e.Right)
		if err != nil {
			return nil, errors.Wrap(err, "assignment rhs")
		}

		return ir.BinOp{
			Op:    string(e.Op),
			Left:  l,
			Right: r,
		}, nil
	default:
		return nil, errors.New("unsupported type: %T", e)
	}
}

func newBlock(prev scope) *block {
	return &block{
		prev:  prev,
		vars:  map[string]int{},
		graph: map[ir.Var]ir.Expr{},
	}
}

func (b *block) get(name string) (r ir.Var) {
	if b == nil {
		return ir.Var{Name: name}
	}

	if id, ok := b.vars[name]; ok {
		return ir.Var{Name: name, ID: id}
	}

	if b.prev == nil {
		return ir.Var{Name: name}
	}

	return b.prev.get(name)
}

func (b *block) new(name string) (r ir.Var) {
	r = b.get(name)
	r.ID++
	b.vars[name] = r.ID

	return r
}

func (b *block) put(name string, e ir.Expr) (r ir.Var) {
	r = b.new(name)

	b.graph[r] = e

	return r
}

func (b *block) expr(v ir.Var) (e ir.Expr) {
	if e, ok := b.graph[v]; ok {
		return e
	}

	if b.prev == nil {
		return nil
	}

	return b.prev.expr(v)
}

func (b *block) TlogAppend(e *wire.Encoder, buf []byte) []byte {
	buf = e.AppendMap(buf, 1)
	buf = e.AppendKeyValue(buf, "vars", b.vars)
	return buf
}

func (b *ifstmt) get(name string) (r ir.Var) {
	if r = b.front.get(name); r.ID != 0 {
		return r
	}

	q := b.then.get(name)
	w := b.alt.get(name)

	if q == w {
		return q
	}

	if b.front == nil {
		b.front = newBlock(nil)
	}

	return b.front.put(name, ir.Phi{
		Cond: b.cond,
		Then: q,
		Else: w,
	})
}

func (b *ifstmt) new(name string) ir.Var {
	panic("nope")
}

func (b *ifstmt) put(name string, e ir.Expr) (r ir.Var) {
	panic("nope")
}

func (b *ifstmt) expr(v ir.Var) (e ir.Expr) {
	if b.front != nil {
		if e, ok := b.front.graph[v]; ok {
			return e
		}
	}

	if e = b.then.expr(v); e != nil {
		return e
	}

	return b.alt.expr(v)
}
