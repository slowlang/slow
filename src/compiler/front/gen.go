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
	FuncScope struct {
		Func *ir.Func

		ByVal map[ast.Expr]int
	}

	Scope struct {
		*FuncScope

		Vars map[ast.Ident]int
	}
)

func (c *Front) Analyze(ctx context.Context) (p *ir.Package, err error) {
	p = &ir.Package{}

	for _, f := range c.files {
		for _, ff := range f.parsed.Funcs {
			fc, err := c.analyzeFunc(ctx, ff)
			if err != nil {
				return nil, errors.Wrap(err, "%v", ff.Name)
			}

			p.Funcs = append(p.Funcs, fc)
		}
	}

	return
}

func (c *Front) analyzeFunc(ctx context.Context, f *ast.Func) (fc *ir.Func, err error) {
	fc = &ir.Func{
		Name: f.Name,
	}

	fs := &FuncScope{
		Func:  fc,
		ByVal: map[ast.Expr]int{},
	}

	s := newScope(fs)

	fc.Args = make([]*ir.Type, len(f.Args))

	for i, arg := range f.Args {
		a := ir.Arg{Num: i}

		//	fc.Args[i] = a
		id := s.newExpr(a)
		//	s.Func.Exprs[id].Used = 0

		s.Vars[arg.Name] = id
	}

	_, err = c.analyzeBlock(ctx, s, f.Body, -1)
	if err != nil {
		return nil, errors.Wrap(err, "body")
	}

	for id, e := range fc.Exprs {
		tlog.Printw("func expr", "func", f.Name, "id", id, "expr", e)
	}

	for id, b := range fc.Blocks {
		tlog.Printw("func block", "func", f.Name, "id", id, "block", b)
	}

	return fc, nil
}

func (c *Front) analyzeBlock(ctx context.Context, s *Scope, b *ast.Block, next int) (entry int, err error) {
	bid := s.newBlock()
	entry = bid

	for i, x := range b.Stmts {
		switch x := x.(type) {
		case ast.Assignment:
			r, err := c.calcRhsExpr(ctx, s, x.Rhs)
			if err != nil {
				return 0, errors.Wrap(err, "assign: rhs")
			}

			switch l := x.Lhs.(type) {
			case ast.Ident:
				s.Vars[l] = r
			//	s.Func.Exprs[r].Used--
			default:
				panic(l)
			}
		case ast.Return:
			if i+1 != len(b.Stmts) {
				return 0, errors.New("unreachable code")
			}

			r, err := c.calcRhsExpr(ctx, s, x.Value)
			if err != nil {
				return 0, errors.Wrap(err, "return value")
			}

			s.addStmt(bid, ir.Ret{Value: r})
		case *ast.IfStmt:
			r, err := c.calcRhsExpr(ctx, s, x.Cond)
			if err != nil {
				return 0, errors.Wrap(err, "if: cond")
			}

			next := s.newBlock()

			then, err := c.analyzeBlock(ctx, s, x.Then, next)
			if err != nil {
				return 0, errors.Wrap(err, "body")
			}

			ifs := ir.If{
				Cond: r,
				Then: then,
			}

			s.addStmt(bid, ifs)

			s.block(bid).Next = next
			bid = next
		default:
			return 0, errors.New("unsupported type: %T", x)
		}
	}

	s.block(bid).Next = next

	return entry, nil
}

func (c *Front) calcRhsExpr(ctx context.Context, s *Scope, e ast.Expr) (id int, err error) {
	//	defer func() {
	//		tlog.Printw("calc expr", "expr", e, "id", id, "used", s.Func.Exprs[id].Used, "from", loc.Caller(1))
	//	}()

	//	defer func() {
	//		s.Func.Exprs[id].Used++
	//	}()

	if id, ok := s.ByVal[e]; ok {
		return id, nil
	}

	switch e := e.(type) {
	case ast.Ident:
		id, ok := s.Vars[e]
		if !ok {
			return 0, errors.New("unknown var: %v", e)
		}

		return id, nil
	case ast.Number:
		v, err := strconv.ParseInt(string(e), 10, 64)
		if err != nil {
			return 0, errors.Wrap(err, "parse int")
		}

		if id, ok := s.ByVal[v]; ok {
			return id, nil
		}

		id = s.newExpr(v)

		return id, nil
	case ast.BinOp:
		l, err := c.calcRhsExpr(ctx, s, e.Left)
		if err != nil {
			return 0, errors.Wrap(err, "binop left")
		}

		r, err := c.calcRhsExpr(ctx, s, e.Right)
		if err != nil {
			return 0, errors.Wrap(err, "binop right")
		}

		v := ir.BinOp{
			Op:    string(e.Op),
			Left:  l,
			Right: r,
		}

		if id, ok := s.ByVal[v]; ok {
			return id, nil
		}

		id = s.newExpr(v)

		s.uses(id, l, r)

		return id, nil
	default:
		return 0, errors.New("unsupported expr: %T", e)
	}

	return
}

func newScope(f *FuncScope) *Scope {
	return &Scope{
		FuncScope: f,
		Vars:      map[ast.Ident]int{},
	}
}

func (s *Scope) newExpr(v any) (id int) {
	id = len(s.Func.Exprs)

	x := ir.Expr{
		Value: v,
	}

	s.Func.Exprs = append(s.Func.Exprs, x)
	s.ByVal[v] = id

	return id
}

func (s *Scope) uses(id int, uses ...int) {
	for _, u := range uses {
		s.Func.Exprs[u].UsedBy = append(s.Func.Exprs[u].UsedBy, id)
	}
}

func (s *Scope) newBlock() (id int) {
	id = len(s.Func.Blocks)
	b := ir.Block{}

	s.Func.Blocks = append(s.Func.Blocks, b)

	return id
}

func (s *Scope) block(id int) *ir.Block {
	return &s.Func.Blocks[id]
}

func (s *Scope) addStmt(id int, st ir.Stmt) {
	b := s.block(id)
	b.Stmts = append(b.Stmts, st)
}
