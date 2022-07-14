package front

import (
	"context"
	"strconv"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"
	"github.com/nikandfor/tlog/wire"
	"github.com/slowlang/slow/src/compiler/ast"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Value struct {
		Name string
	}

	FuncState struct {
		Expr map[ir.Expr]ir.Expr

		BaseID int
	}

	State struct {
		Func   *FuncState
		Parent *State

		Vars map[string]int

		Cond ir.Expr
		Then *State
		Else *State

		Return ir.Expr
	}

	Var struct {
		Name string
		ID   int
	}
)

func (fc *Front) Compile(ctx context.Context) (err error) {
	f := fc.files[0].parsed

	for _, f := range f.Funcs {
		ff, err := fc.compileFunc(ctx, nil, f)
		if err != nil {
			return errors.Wrap(err, "%v", f.Name)
		}

		tlog.SpanFromContext(ctx).Printw("func", "ff", ff)
	}

	return nil
}

func (fc *Front) compileFunc(ctx context.Context, root *State, f *ast.Func) (ff *ir.Func, err error) {
	s := NewState(nil)
	s.Func = &FuncState{
		Expr: map[ir.Expr]ir.Expr{},
	}

	for i, a := range f.Args {
		v := s.new(a.Name)
		s.set(v, ir.Arg{Num: i})
	}

	ss, err := fc.compileBlock(ctx, s, f.Body)
	if err != nil {
		return nil, errors.Wrap(err, "block")
	}

	tlog.SpanFromContext(ctx).Printw("func state", "s", ss)

	for s := ss; s != nil; s = s.Parent {
		tlog.SpanFromContext(ctx).Printw("state graph",
			"ptr", tlog.FormatNext("%p"), s,
			"vars", s.Vars,
			"parent", tlog.FormatNext("%p"), s.Parent,
			"cond", s.Cond,
			"then", tlog.FormatNext("%p"), s.Then,
			"ret", s.Return,
		)

		if s := s.Then; s != nil {
			tlog.SpanFromContext(ctx).Printw("state graph then",
				"ptr", tlog.FormatNext("%p"), s,
				"vars", s.Vars,
				"parent", tlog.FormatNext("%p"), s.Parent,
				"cond", s.Cond,
				"then", tlog.FormatNext("%p"), s.Then,
				"ret", s.Return,
			)
		}
	}

	for l, r := range s.Func.Expr {
		tlog.SpanFromContext(ctx).Printw("state", "l", l, "r", r, "r_type", tlog.FormatNext("%T"), r)
	}

	return nil, nil
}

func (fc *Front) compileBlock(ctx context.Context, s *State, b *ast.Block) (_ *State, err error) {
loop:
	for _, x := range b.Stmts {
		switch x := x.(type) {
		case ast.Assignment:
			r, err := fc.calcRhsExpr(ctx, s, x.Rhs)
			if err != nil {
				return nil, errors.Wrap(err, "assignment rhs")
			}

			l, err := fc.calcLhsExpr(ctx, s, x.Lhs)
			if err != nil {
				return nil, errors.Wrap(err, "assignment lhs")
			}

			s.set(l, r)
		case ast.Return:
			r, err := fc.calcRhsExpr(ctx, s, x.Value)
			if err != nil {
				return nil, errors.Wrap(err, "return expr")
			}

			s.Return = r

			break loop
		case *ast.IfStmt:
			c, err := fc.calcRhsExpr(ctx, s, x.Cond)
			if err != nil {
				return nil, errors.Wrap(err, "if cond")
			}

			ts := NewState(s)
			t, err := fc.compileBlock(ctx, ts, x.Then)
			tlog.Printw("then parsed", "ptr_the_sane", t == ts, "err", err, "err_nil", err == nil)
			if err != nil {
				return nil, errors.Wrap(err, "if then")
			}

			next := NewState(s)

			next.Cond = c
			next.Then = t
			next.Else = s

			s = next
		default:
			return nil, errors.New("unsupported type: %T", x)
		}
	}

	return s, nil
}

func (fc *Front) calcLhsExpr(ctx context.Context, s *State, e ast.Expr) (x ir.Expr, err error) {
	switch e := e.(type) {
	case ast.Ident:
		return s.new(string(e)), nil
	default:
		return nil, errors.New("unsupported type: %T", e)
	}
}

func (fc *Front) calcRhsExpr(ctx context.Context, s *State, e ast.Expr) (x ir.Expr, err error) {
	switch e := e.(type) {
	case ast.Ident:
		return s.get(string(e)), nil
	case ast.Number:
		v, err := strconv.ParseInt(string(e), 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "parse num")
		}

		return ir.Word{Value: v}, nil
	case ast.BinOp:
		l, err := fc.calcRhsExpr(ctx, s, e.Left)
		if err != nil {
			return nil, errors.Wrap(err, "assignment lhs")
		}

		r, err := fc.calcRhsExpr(ctx, s, e.Right)
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

func NewState(p *State) *State {
	s := &State{
		Parent: p,
		Vars:   make(map[string]int),
	}

	tlog.Printw("new state", "ptr", tlog.FormatNext("%p"), s, "parent", tlog.FormatNext("%p"), p, "from", loc.Callers(1, 3))

	if p != nil {
		s.Func = p.Func
	}

	return s
}

func (s *State) set(l, r ir.Expr) {
	tlog.Printw("set func expr", "ptr", tlog.FormatNext("%p"), s, "l", l, "r", r, "from", loc.Callers(1, 3))
	s.Func.Expr[l] = r
}

func (s *State) new(name string) (v ir.Var) {
	v = ir.Var{Name: name, ID: s.nextid()}
	s.Vars[name] = v.ID
	return v
}

func (s *State) get(name string) (v ir.Var) {
	defer func() {
		tlog.Printw("get var", "ptr", tlog.FormatNext("%p"), s, "name", name, "v", v, "cond", s.Cond, "from", loc.Callers(1, 2))
	}()

	if s == nil {
		return ir.Var{}
	}

	if id, ok := s.Vars[name]; ok {
		return ir.Var{Name: name, ID: id}
	}

	if s.Cond == nil {
		return s.Parent.get(name)
	}

	a := s.Then.get(name)
	b := s.Else.get(name)

	tlog.Printw("get cond var", "ptr", tlog.FormatNext("%p"), s, "a", a, "b", b)

	if a == (ir.Var{}) && a == (ir.Var{}) {
		return s.Parent.get(name)
	}

	if a.ID == b.ID {
		return a
	}

	if a == (ir.Var{}) {
		return b
	}

	if b == (ir.Var{}) {
		return a
	}

	v = s.new(name)

	s.set(v, ir.Phi{
		Cond: s.Cond,
		Then: a,
		Else: b,
	})

	return v
}

func (s *State) nextid() int {
	s.Func.BaseID++

	return s.Func.BaseID
}

func (s *State) TlogAppend(e *wire.Encoder, b []byte) []byte {
	if s == nil {
		return e.AppendNil(b)
	}

	b = e.AppendMap(b, -1)

	if s.Return != nil {
		b = e.AppendKeyValue(b, "return", s.Return)
	}

	if s.Cond != nil {
		b = e.AppendKeyValue(b, "cond", s.Cond)
	}

	b = e.AppendKey(b, "vars")
	b = e.AppendMap(b, len(s.Vars))

	for n, id := range s.Vars {
		b = e.AppendKeyValue(b, n, id)
	}

	if s.Parent != nil {
		b = e.AppendKey(b, "parent")
		b = s.Parent.TlogAppend(e, b)
	}

	if s.Func != nil {
		b = e.AppendKey(b, "func")
		b = s.Func.TlogAppend(e, b)
	}

	b = e.AppendBreak(b)

	return b
}

func (s *FuncState) TlogAppend(e *wire.Encoder, b []byte) []byte {
	b = e.AppendMap(b, -1)

	b = e.AppendKey(b, "expr")
	b = e.AppendArray(b, len(s.Expr))

	for l, r := range s.Expr {
		b = e.AppendMap(b, 2)
		b = e.AppendKeyValue(b, "l", l)
		b = e.AppendKeyValue(b, "r", r)
	}

	b = e.AppendBreak(b)

	return b
}
