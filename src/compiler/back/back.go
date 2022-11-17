package back

import (
	"context"

	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Back struct{}

	Arch struct {
		Regs []int
		Expr map[int]int // expr -> reg
		Used map[int]int // expr -> count
	}
)

func New() *Back {
	return &Back{}
}

func (c *Back) CompilePackage(ctx context.Context, b []byte, p *ir.Package) (_ []byte, err error) {
	//	a := NewArch()

	//	return c.compileFunc(ctx, a, b, p.Funcs[0])
	return
}

/*
func (c *Back) compileFunc(ctx context.Context, a *Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	for i := range f.Args {
		a.Alloc(i, i)
	}

	b = hfmt.AppendPrintf(b, `
.global _%s
.align 4
_%[1]s:
	STP	FP, LR, [SP, #-16]!
	MOV	FP, SP

body:
`, f.Name)

	for _, bl := range f.Blocks {
		b, err = c.compileBlock(ctx, a, b, f, bl)
		if err != nil {
			return nil, errors.Wrap(err, "block")
		}
	}

	b = append(b, `
return:
	LDP	FP, LR, [SP], #16
	RET
`...)

	return b, nil
}

func (c *Back) compileBlock(ctx context.Context, a *Arch, b []byte, f *ir.Func, block *ir.Block) (_ []byte, err error) {
	b = hfmt.AppendPrintf(b, "block_%v:\n", block.ID)

	for _, s := range block.Stmts {
		switch s := s.(type) {
		case ir.Ret:
			b, err = c.compileExpr(ctx, a, b, f, f.Exprs[s.Value])
			if err != nil {
				return nil, errors.Wrap(err, "return")
			}

			if a.Expr[s.Value] != 0 {
				a.EnsureFree(0)

				b = hfmt.AppendPrintf(b, "	MOV	X%d, X%d\n", 0, a.Expr[s.Value])
				b = hfmt.AppendPrintf(b, "	B	return\n")
			}

			if a.Used[s.Value] == f.Exprs[s.Value].Used {
				a.FreeExpr(s.Value)
			}
		case ir.IfStmt:
			b, err = c.compileExpr(ctx, a, b, f, f.Exprs[s.Cond])
			if err != nil {
				return nil, errors.Wrap(err, "return")
			}

			b = hfmt.AppendPrintf(b, "	TBNZ	X%d, #0, block_%v\n", a.Expr[s.Cond], s.Branch)
			b = hfmt.AppendPrintf(b, "	B	block_%v\n", block.Next)
		default:
			return nil, errors.New("unsupported stmt: %T", s)
		}
	}

	return b, nil
}

func (c *Back) compileExpr(ctx context.Context, a *Arch, b []byte, f *ir.Func, e *ir.Expr) (_ []byte, err error) {
	if _, ok := a.Expr[e.ID]; ok {
		a.Used[e.ID]++

		return b, nil
	}

	switch v := e.Value.(type) {
	//case ir.Arg:
	//	e.Reg = v.Num
	case int64:
		reg := a.AllocExpr(e.ID)

		b = compileConst(ctx, b, reg, v)
	case ir.BinOp:
		b, err = c.compileExpr(ctx, a, b, f, f.Exprs[v.Left])
		if err != nil {
			return nil, errors.Wrap(err, "left")
		}

		b, err = c.compileExpr(ctx, a, b, f, f.Exprs[v.Right])
		if err != nil {
			return nil, errors.Wrap(err, "right")
		}

		lreg := a.Expr[v.Left]
		rreg := a.Expr[v.Right]

		if a.Used[v.Left] == f.Exprs[v.Left].Used {
			a.FreeExpr(v.Left)
		}

		if a.Used[v.Right] == f.Exprs[v.Right].Used {
			a.FreeExpr(v.Right)
		}

		reg := a.AllocExpr(e.ID)

		switch v.Op {
		case "+":
			b = hfmt.AppendPrintf(b, "	ADD	X%d, X%d, X%d	; expr %d\n", reg, lreg, rreg, e.ID)
		case "<":
			b = hfmt.AppendPrintf(b, "	CMP	X%d, X%d	; expr %d\n", lreg, rreg, e.ID)
			b = hfmt.AppendPrintf(b, "	CSET	X%d, LT	; expr %d\n", reg, e.ID)
		case ">":
			b = hfmt.AppendPrintf(b, "	CMP	X%d, X%d	; expr %d\n", lreg, rreg, e.ID)
			b = hfmt.AppendPrintf(b, "	CSET	X%d, GT	; expr %d\n", reg, e.ID)
		default:
			panic(v.Op)
		}
	default:
		return nil, errors.New("unsupported expr: %T", v)
	}

	a.Used[e.ID]++

	return b, nil
}

func compileConst(ctx context.Context, b []byte, reg int, y int64) (_ []byte) {
	b = hfmt.AppendPrintf(b, "	MOV	X%v, #0x%x	; %[2]d\n", reg, y&0xffff)

	for sh := 0; y > 0xffff; sh += 16 {
		y >>= 16

		b = hfmt.AppendPrintf(b, "	MOV	X%v, #0x%x, LSL #%d	; %[2]d\n", reg, y&0xffff, sh)
	}

	return b
}

func NewArch() *Arch {
	a := &Arch{
		Regs: make([]int, 30),
		Expr: make(map[int]int),
		Used: make(map[int]int),
	}

	for i := range a.Regs {
		a.Regs[i] = len(a.Regs) - 1 - i
	}

	return a
}

func (a *Arch) AllocExpr(e int) (r int) {
	if r, ok := a.Expr[e]; ok {
		tlog.V("reg,reg_alloc").Printw("reuse reg", "reg", r, "expr", e, "from", loc.Callers(1, 2))
		return r
	}

	l := len(a.Regs) - 1
	r = a.Regs[l]
	a.Regs = a.Regs[:l]

	a.Expr[e] = r

	tlog.V("reg,reg_alloc").Printw("alloc reg", "reg", r, "expr", e, "from", loc.Callers(1, 2))

	return r
}

func (a *Arch) Alloc(r, e int) {
	for i, f := range a.Regs {
		if f == r {
			a.Expr[e] = r
			tlog.V("reg,reg_alloc").Printw("alloc reg", "reg", r, "expr", e, "from", loc.Callers(1, 2))

			l := len(a.Regs) - 1
			if i < l {
				copy(a.Regs[i:], a.Regs[i+1:])
			}
			a.Regs = a.Regs[:l]

			return
		}
	}

	panic("register is not available")
}

func (a *Arch) FreeExpr(e int) {
	r, ok := a.Expr[e]
	if !ok {
		panic("unknown expr freed")
	}

	a.Regs = append(a.Regs, r)
	delete(a.Expr, e)
	//	delete(a.Used, e)

	tlog.V("reg,reg_free").Printw("free reg", "reg", r, "expr", e, "from", loc.Callers(1, 2))
}

func (a *Arch) EnsureFree(r int) {
	for _, f := range a.Regs {
		if f == r {
			return
		}
	}

	panic("register is not available")
}
*/
