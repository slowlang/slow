package front

import (
	"context"
	"fmt"
	"io"
	"os"
	"strconv"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"
	"github.com/slowlang/slow/src/compiler/ast"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	CalcExpr struct {
		ID     int
		Uses   map[int]struct{}
		UsedBy map[int]struct{}
		Value  Expr

		//	Cost Expr
		//	Regs Expr

		Reg  int
		Used int
	}

	FuncScope struct {
		Name string

		Expr  []*CalcExpr
		ByVal map[Expr]int

		Return int

		State *State

		Arch *Arch
	}

	State struct {
		Func *FuncScope
		Prev *State

		Vars map[ast.Ident]int

		Return []int

		Cond int
		Alt  *State
	}

	CalcBinOp struct {
		Op   ast.Op
		L, R int
	}

	Phi struct {
		Cond int
		Then int
		Else int
	}
)

func (fc *Front) Analyze(ctx context.Context) (err error) {
	f := fc.files[0].parsed

	for _, f := range f.Funcs {
		ff, err := fc.analyzeFunc(ctx, nil, f)
		if err != nil {
			return errors.Wrap(err, "%v", f.Name)
		}

		tlog.SpanFromContext(ctx).Printw("func", "ff", ff)

		fc.files[0].analyzed = append(fc.files[0].analyzed, ff)
	}

	return nil
}

func (fc *Front) analyzeFunc(ctx context.Context, parentScope any, f *ast.Func) (ff *FuncScope, err error) {
	ff = &FuncScope{
		Name:  f.Name,
		ByVal: map[Expr]int{},
	}

	s := &State{
		Func: ff,
		Vars: map[ast.Ident]int{},
	}

	for i, a := range f.Args {
		e := ff.newExpr(ir.Arg{Num: i})
		s.Vars[a.Name] = e.ID
	}

	ss, err := fc.analyzeBlock(ctx, s, f.Body)
	if err != nil {
		return nil, errors.Wrap(err, "block")
	}

	ff.Return = ss.getReturn(nil)
	ff.State = ss

	tlog.Printw("func", "ret", ff.Return)

	for id, e := range ff.Expr {
		tlog.Printw("func expr", "id", id, "expr", e)
	}

	var p func(s, stop *State)
	p = func(s, stop *State) {
		if s == nil || s == stop {
			return
		}

		p(s.Prev, stop)
		if s.Alt != nil {
			p(s.Alt, s.Prev)
		}

		tlog.Printw("state block",
			"vars", s.Vars,
			"return", s.Return,
			"ptr", tlog.FormatNext("%p"), s,
			"prev", tlog.FormatNext("%p"), s.Prev,
			"alt", tlog.FormatNext("%p"), s.Alt,
			"cond", s.Cond,
		)
	}

	p(ss, nil)

	ff.dump(os.Stderr, 0, ff.Return)

	return ff, nil
}

func (fc *Front) analyzeBlock(ctx context.Context, s *State, b *ast.Block) (_ *State, err error) {
loop:
	for _, x := range b.Stmts {
		//	statement:
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

			switch l := l.(type) {
			case ast.Ident:
				s.Vars[l] = r.ID
			default:
				panic(l)
			}
		case ast.Return:
			r, err := fc.calcRhsExpr(ctx, s, x.Value)
			if err != nil {
				return nil, errors.Wrap(err, "return value")
			}

			//r.UsedBy[-1] = struct{}{}

			s.Return = append(s.Return, r.ID)

			tlog.Printw("return", "s", tlog.FormatNext("%p"), s, "from", loc.Callers(1, 2))

			break loop
		case *ast.IfStmt:
			r, err := fc.calcRhsExpr(ctx, s, x.Cond)
			if err != nil {
				return nil, errors.Wrap(err, "if cond")
			}

			alt := &State{
				Prev: s,
				Func: s.Func,
				Vars: map[ast.Ident]int{},
			}

			t, err := fc.analyzeBlock(ctx, alt, x.Then)
			if err != nil {
				return nil, errors.Wrap(err, "if then")
			}

			/*
				switch v := r.Value.(type) {
				case bool:
					if v {
						s = t
					}

					break statement
				}
			*/

			next := &State{
				Prev: s,
				Func: s.Func,
				Vars: map[ast.Ident]int{},

				Cond: r.ID,
				Alt:  t,
			}

			tlog.Printw("branch",
				"s", tlog.FormatNext("%p"), next,
				"alt", tlog.FormatNext("%p"), t,
				"prev", tlog.FormatNext("%p"), s,
				"from", loc.Callers(1, 2))

			s = next
		default:
			return nil, errors.New("unsupported type: %T", x)
		}
	}

	return s, nil
}

func (fc *Front) calcRhsExpr(ctx context.Context, s *State, e ast.Expr) (x *CalcExpr, err error) {
	if id, ok := s.Func.ByVal[e]; ok {
		return s.Func.Expr[id], nil
	}

	switch e := e.(type) {
	case ast.Ident:
		id, err := s.getVar(e)
		if err != nil {
			return nil, errors.Wrap(err, "ident")
		}

		x = s.Func.Expr[id]

		return x, nil
	case ast.Number:
		v, err := strconv.ParseInt(string(e), 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "parse int")
		}

		if id, ok := s.Func.ByVal[ir.Word{Value: v}]; ok {
			return s.Func.Expr[id], nil
		}

		x = s.Func.newExpr(ir.Word{Value: v})

		return x, nil
	case ast.BinOp:
		l, err := fc.calcRhsExpr(ctx, s, e.Left)
		if err != nil {
			return nil, errors.Wrap(err, "binop left")
		}

		r, err := fc.calcRhsExpr(ctx, s, e.Right)
		if err != nil {
			return nil, errors.Wrap(err, "binop right")
		}

		q, err := fc.Precalc(ctx, s, e.Op, l, r)
		if err != nil {
			return nil, errors.Wrap(err, "calc const")
		}

		if id, ok := s.Func.ByVal[q]; ok {
			return s.Func.Expr[id], nil
		}

		x = s.Func.newExpr(q)

		x.Uses[l.ID] = struct{}{}
		x.Uses[r.ID] = struct{}{}
		l.UsedBy[x.ID] = struct{}{}
		r.UsedBy[x.ID] = struct{}{}

		return x, nil
	default:
		return nil, errors.New("unsupported type: %T", e)
	}
}

func (fc *Front) calcLhsExpr(ctx context.Context, s *State, e ast.Expr) (x Expr, err error) {
	switch e := e.(type) {
	case ast.Ident:
		return e, nil
	default:
		return nil, errors.New("unsupported type: %T", e)
	}
}

func (fc *Front) Precalc(ctx context.Context, s *State, op ast.Op, l, r *CalcExpr) (x Expr, err error) {
	x = CalcBinOp{
		Op: op,
		L:  l.ID,
		R:  r.ID,
	}

	return x, nil

	a, ok := l.Value.(ir.Word)
	if !ok {
		return
	}

	b, ok := r.Value.(ir.Word)
	if !ok {
		return
	}

	switch op {
	case "<":
		x = a.Value < b.Value
	case ">":
		x = a.Value > b.Value
	case "+":
		x = ir.Word{Value: a.Value + b.Value}
	}

	return
}

func (s *State) getVar(n ast.Ident) (id int, err error) {
	for q := s; q != nil; q = q.Prev {
		if id, ok := q.Vars[n]; ok {
			return id, nil
		}

		var alt int = -1
		for a := q.Alt; a != nil && a != q.Prev; a = a.Prev {
			if id, ok := a.Vars[n]; ok {
				alt = id
				break
			}
		}

		if alt < 0 {
			continue
		}

		base, err := q.Prev.getVar(n)
		if err != nil {
			return 0, err
		}

		id = s.makePhi(q.Cond, alt, base)
		s.Vars[n] = id

		return id, nil
	}

	return 0, errors.New("unknown var: %v", n)
}

func (s *State) getReturn(stop *State) (id int) {
	id = s.Return[0]

	for q := s; q != nil && q != stop; q = q.Prev {
		if q.Alt == nil || len(q.Alt.Return) == 0 {
			continue
		}

		alt := q.Alt.getReturn(q.Prev)

		tlog.Printw("make return", "cond", q.Cond, "then", alt, "else", id, "from", loc.Callers(1, 3))

		id = s.makePhi(q.Cond, alt, id)
	}

	return
}

func (s *State) makePhi(cond, then, els int) int {
	p := Phi{
		Cond: cond,
		Then: then,
		Else: els,
	}

	xid, ok := s.Func.ByVal[p]
	if ok {
		return xid
	}

	x := s.Func.newExpr(p)
	xid = x.ID

	x.Uses[cond] = struct{}{}
	x.Uses[then] = struct{}{}
	x.Uses[els] = struct{}{}

	s.Func.Expr[cond].UsedBy[xid] = struct{}{}
	s.Func.Expr[then].UsedBy[xid] = struct{}{}
	s.Func.Expr[els].UsedBy[xid] = struct{}{}

	return xid
}

func (ff *FuncScope) newExpr(v Expr) *CalcExpr {
	//	if id, ok := ff.ByVal[v]; ok {
	//		return ff.Expr[id]
	//	}

	e := &CalcExpr{
		ID:     len(ff.Expr),
		Uses:   map[int]struct{}{},
		UsedBy: map[int]struct{}{},
		Value:  v,
	}
	ff.Expr = append(ff.Expr, e)
	ff.ByVal[v] = e.ID
	return e
}

func (ff *FuncScope) dump(w io.Writer, d, t int) {
	e := ff.Expr[t]

	fmt.Fprintf(w, "%2d  %v", t, "\t\t\t\t\t\t\t\t\t"[:d])

	switch v := e.Value.(type) {
	case ir.Arg, ir.Word:
		fmt.Fprintf(w, "%T %[1]v\n", v)
	case CalcBinOp:
		fmt.Fprintf(w, "%v\n", v.Op)
		ff.dump(w, d+1, v.L)
		ff.dump(w, d+1, v.R)
	case Phi:
		fmt.Fprintf(w, "phi\n")
		ff.dump(w, d+1, v.Cond)
		ff.dump(w, d+1, v.Then)
		ff.dump(w, d+1, v.Else)
	default:
		fmt.Fprintf(w, "wtf: %T\n", v)
	}
}
