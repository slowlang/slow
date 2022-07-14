package back

import (
	"context"

	"github.com/nikandfor/hacked/hfmt"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Arch interface{}
)

func Compile(ctx context.Context, p ir.Package) (asm []byte, err error) {
	panic("qwe")
}

func compileFunc(ctx context.Context, b []byte, f ir.Func) ([]byte, error) {
	b = hfmt.AppendPrintf(b, `
.align 4
.global _%s
_%[1]s:
	STP	FP, LR, [SP, #-16]!
	MOV	FP, SP

`, f.Name)

	// todo

	b = append(b, `
	LDP	FP, LR, [SP], #16
	RET
`...)

	return b, nil
}

func compileConst(ctx context.Context, b []byte, reg int, y ir.Word) (_ []byte) {
	b = hfmt.AppendPrintf(b, "	MOV	X%v, #%d\n", reg, y.Value&0xffff)

	for sh := 0; y.Value > 0xffff; sh += 16 {
		y.Value >>= 16

		b = hfmt.AppendPrintf(b, "	MOV	X%v, #%d, LSL #%d // Word\n", reg, y.Value&0xffff, sh)
	}

	return b
}

/*
func (s *State) Compile(ctx context.Context, a Arch) ([]byte, error) {
	b, err := s.compileFunc(ctx, nil, a, s.prog.Funcs[0])
	if err != nil {
		return nil, err
	}

	return b, nil
}

func (s *State) compileFunc(ctx context.Context, b []byte, a Arch, f *Func) ([]byte, error) {
	b = hfmt.AppendPrintf(b, `
.align 4
.global _%s
_%[1]s:
	STP	FP, LR, [SP, #-16]!
	MOV	FP, SP

`, f.Name)

	r, ok := f.syms[string(f.RetArgs[0].Name)]
	if !ok {
		return nil, errors.New("no expr for ret arg")
	}

	b, err := s.compileExpr(ctx, b, a, f, 0, r)
	if err != nil {
		return nil, err
	}

	b = append(b, `
	LDP	FP, LR, [SP], #16
	RET
`...)

	return b, nil
}

func (s *State) compileExpr(ctx context.Context, b []byte, a Arch, f *Func, reg int, e Expr) (_ []byte, err error) {
	switch e := e.(type) {
	case Word:
		b = s.compileConst(ctx, b, a, reg, e)
	case Arg:
		if reg == e.Num {
			return b, nil
		}

		b = hfmt.AppendPrintf(b, "	MOV	X%d, X%d\n", reg, e.Num)
	case BinOp:
		b, err = s.compileExpr(ctx, b, a, f, reg+1, e.Left)
		if err != nil {
			return nil, errors.Wrap(err, "left")
		}

		switch e.Op {
		case "+":
			if y, ok := e.Right.(Word); ok && y.Value == 0 {
				// nothing to do
			} else if y.Value > 0 && y.Value < 1<<12 {
				b = hfmt.AppendPrintf(b, "	ADD	X%d, X%d, #%d\n", reg, reg+1, y.Value)
			} else {
				b, err = s.compileExpr(ctx, b, a, f, reg+2, e.Right)
				if err != nil {
					return nil, errors.Wrap(err, "right")
				}

				b = hfmt.AppendPrintf(b, "	ADD	X%d, X%d, X%d\n", reg, reg+1, reg+2)
			}
		default:
			return nil, errors.New("unsupported op: %v", e.Op)
		}
	default:
		panic(e)
	}

	return b, nil
}

func (s *State) compileConst(ctx context.Context, b []byte, a Arch, reg int, y Word) (_ []byte) {
	b = hfmt.AppendPrintf(b, "	MOV	X%v, #%d\n", reg, y.Value&0xffff)

	for sh := 0; y.Value > 0xffff; sh += 16 {
		y.Value >>= 16

		b = hfmt.AppendPrintf(b, "	MOV	X%v, #%d, LSL #%d // Word\n", reg, y.Value&0xffff, sh)
	}

	return b
}
*/

/*
func (s *State) Analyze(ctx context.Context) error {
	err := s.analyzeFunc(ctx, s.prog.Funcs[0])
	if err != nil {
		return err
	}

	return nil
}

func (s *State) analyzeFunc(ctx context.Context, f *Func) (ir.Func, error) {
	f.syms = make(map[string]Expr)

	for _, a := range f.Args {
		f.syms[string(a.Name)] = a
	}

	err := s.analyzeBlock(ctx, f, f.Body)
	if err != nil {
		return err
	}

	ret, ok := f.syms[string(f.RetArgs[0].Name)]
	if !ok {
		return errors.New("no result")
	}

	tlog.SpanFromContext(ctx).Printw("func", "name", f.Name, "result", ret)
	tlog.SpanFromContext(ctx).Printw("func", "syms", f.syms)

	return nil
}

func (s *State) analyzeBlock(ctx context.Context, f *Func, b *Block) (err error) {
	for _, st := range b.Stmts {
		var pos int

		switch st := st.(type) {
		case Assignment:
			pos = st.Pos
			err = s.analyzeAssignment(ctx, f, st)
		case Return:
			pos = st.Pos
			err = s.analyzeReturn(ctx, f, st)
		default:
			return errors.New("unsupported statement: %T", st)
		}

		if err != nil {
			return errors.Wrap(err, "at pos 0x%x", pos)
		}
	}

	return nil
}

func (s *State) analyzeReturn(ctx context.Context, f *Func, r Return) error {
	e, err := s.analyzeRhsExpr(ctx, f, r.Value)
	if err != nil {
		return err
	}

	tlog.SpanFromContext(ctx).Printw("return", "val", r.Value, "expr", e)

	f.syms[string(f.RetArgs[0].Name)] = e

	return nil
}

func (s *State) analyzeAssignment(ctx context.Context, f *Func, a Assignment) error {
	name, err := s.analyzeLhsExpr(ctx, f, a.Lhs)
	if err != nil {
		return err
	}

	e, err := s.analyzeRhsExpr(ctx, f, a.Rhs)
	if err != nil {
		return err
	}

	f.syms[string(name.(Ident))] = e

	tlog.SpanFromContext(ctx).Printw("assign", "lhs", a.Lhs, "rhs", a.Rhs, "expr", e)

	return nil
}

func (s *State) analyzeLhsExpr(ctx context.Context, f *Func, e Expr) (Expr, error) {
	switch e := e.(type) {
	case Ident:
		return e, nil
	case Number:
		return nil, errors.New("rhs expected, got %v", e)
	default:
		panic(e)
	}
}

func (s *State) analyzeRhsExpr(ctx context.Context, f *Func, e Expr) (Expr, error) {
	switch e := e.(type) {
	case Word:
		return e, nil
	case Number:
		v, err := strconv.ParseInt(string(e), 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "parse int")
		}

		return Word{Value: v}, nil
	case Ident:
		r, ok := f.syms[string(e)]
		if !ok {
			return nil, errors.New("unknown symbol: %q", e)
		}

		return s.analyzeRhsExpr(ctx, f, r)
	case Arg:
		return e, nil
	case BinOp:
		l, err := s.analyzeRhsExpr(ctx, f, e.Left)
		if err != nil {
			return nil, errors.Wrap(err, "analyze left")
		}

		r, err := s.analyzeRhsExpr(ctx, f, e.Right)
		if err != nil {
			return nil, errors.Wrap(err, "analyze right")
		}

		return BinOp{Op: e.Op, Left: l, Right: r}, nil
	default:
		panic(e)
	}
}
*/
