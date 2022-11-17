//go:build ignore

package front

import (
	"context"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/hacked/hfmt"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Arch struct {
		Args int
		Regs [100]byte
	}
)

func (fc *Front) Compile(ctx context.Context, obj []byte) (_ []byte, err error) {
	f := fc.files[0]

	for _, f := range f.analyzed {
		obj, err = fc.compileFunc(ctx, obj, f)
		if err != nil {
			return nil, errors.Wrap(err, "%v", f.Name)
		}
	}

	return obj, nil
}

func (fc *Front) compileFunc(ctx context.Context, b []byte, f *FuncScope) (_ []byte, err error) {
	b = hfmt.AppendPrintf(b, `
.align 4
.global _%s
_%[1]s:
	STP	FP, LR, [SP, #-16]!
	MOV	FP, SP

body:
`, f.Name)

	for _, e := range f.Expr {
		e.Reg = -1
	}

	f.Arch = NewAMD64()

	for i := 0; ; i++ {
		id, ok := f.ByVal[ir.Arg{Num: i}]
		if !ok {
			break
		}

		f.Expr[id].Reg = f.Arch.Alloc(i)
	}

	b, err = fc.compileExpr(ctx, b, f, 0, f.Return)
	if err != nil {
		return nil, err
	}

	b = append(b, `
return:
	LDP	FP, LR, [SP], #16
	RET
`...)

	tlog.Printw("arch", "arch", f.Arch)

	return b, nil
}

func (fc *Front) compileState(ctx context.Context, b []byte, f *FuncScope, s, stop *State) (_ []byte, err error) {
	if s == nil || s == stop {
		return b, nil
	}

	b, err = fc.compileState(ctx, b, f, s.Prev, stop)
	if err != nil {
		return nil, err
	}

	if s.Alt != nil {
		b, err = fc.compileState(ctx, b, f, s.Alt, s.Prev)
		if err != nil {
			return nil, err
		}
	}

	tlog.Printw("state", "vars", len(s.Vars), "s", tlog.FormatNext("%p"), s, "from", loc.Callers(1, 4))
	for name, id := range s.Vars {
		reg := -1
		//	reg := f.Arch.Alloc()

		//	_, _, _ = name, id, reg
		tlog.Printw("alloc", "var", name, "expr", id, "reg", reg, "s", tlog.FormatNext("%p"), s, "from", loc.Callers(1, 4))

		//	b, err = fc.compileExpr(ctx, b, f, reg, id)
		//	if err != nil {
		//		return nil, err
		//	}
	}

	if len(s.Return) != 0 {
		b = hfmt.AppendPrintf(b, "	B return\n")
	}

	return b, nil
}

func (fc *Front) compileExpr(ctx context.Context, b []byte, f *FuncScope, reg, t int) (_ []byte, err error) {
	e := f.Expr[t]

	e.Used++

	if e.Reg >= 0 && (e.Reg == reg || reg == -1) {
		return b, nil
	}

	switch v := e.Value.(type) {
	case ir.Word:
		e.Reg = f.Arch.Alloc(reg)

		b = compileConst(ctx, b, e.Reg, v)
	case ir.Arg:
		old := e.Reg
		e.Reg = f.Arch.Alloc(reg)

		b = hfmt.AppendPrintf(b, "	MOV	X%d, X%d	; arg\n", e.Reg, old)
	case Phi:
		b, err = fc.compileCond(ctx, b, f, reg, v.Cond, v.Then, v.Else)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}

		// TODO
		e.Reg = reg
	case CalcBinOp:
		b, err = fc.compileExpr(ctx, b, f, -1, v.L)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}

		b, err = fc.compileExpr(ctx, b, f, -1, v.R)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}

		q := f.Expr[v.L]
		l := q.Reg
		//	if q.Used == len(q.UsedBy) {
		//		f.Arch.Free(q.Reg)
		//		q.Reg = -1
		//	}

		q = f.Expr[v.R]
		r := q.Reg
		//	if q.Used == len(q.UsedBy) {
		//		f.Arch.Free(q.Reg)
		//		q.Reg = -1
		//	}

		e.Reg = f.Arch.Alloc(reg)

		switch v.Op {
		case "+":
			b = hfmt.AppendPrintf(b, "	ADD	X%d, X%d, X%d	; expr %d\n", e.Reg, l, r, t)
		default:
			panic(v)
		}
	default:
		panic(v)
	}

	return b, nil
}

func (fc *Front) compileCond(ctx context.Context, b []byte, f *FuncScope, reg, t, alt, els int) (_ []byte, err error) {
	e := f.Expr[t]

	switch v := e.Value.(type) {
	case CalcBinOp:
		b, err = fc.compileExpr(ctx, b, f, -1, v.L)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}

		b, err = fc.compileExpr(ctx, b, f, -1, v.R)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}

		b = hfmt.AppendPrintf(b, "\nif_%d:\n", t)
		b = hfmt.AppendPrintf(b, "	CMP	X%d, X%d\n", f.Expr[v.L].Reg, f.Expr[v.R].Reg)

		op := ""
		switch v.Op {
		case "<":
			op = "PL"
		case ">":
			op = "LS"
		default:
			panic(v.Op)
		}

		b = hfmt.AppendPrintf(b, "	B%v	skip_%d\n", op, t)

		b, err = fc.compileExpr(ctx, b, f, reg, alt)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}

		b = hfmt.AppendPrintf(b, "skip_%d:\n", t)

		b, err = fc.compileExpr(ctx, b, f, reg, els)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}

		b = hfmt.AppendPrintf(b, "endif_%d:\n\n", t)
	default:
		panic(v)
	}

	return b, nil
}

func compileConst(ctx context.Context, b []byte, reg int, y ir.Word) (_ []byte) {
	b = hfmt.AppendPrintf(b, "	MOV	X%v, #0x%x ; %[2]d\n", reg, y.Value&0xffff)

	for sh := 0; y.Value > 0xffff; sh += 16 {
		y.Value >>= 16

		b = hfmt.AppendPrintf(b, "	MOV	X%v, #0x%x, LSL #%d ; %[2]d\n", reg, y.Value&0xffff, sh)
	}

	return b
}

func NewAMD64() *Arch {
	a := &Arch{
		Args: 9,
	}

	return a
}

func (a *Arch) Alloc(hint int) (r int) {
	defer func() {
		tlog.Printw("alloc register", "reg", r, "hint", hint, "from", loc.Callers(1, 3))
	}()

	if hint >= 0 && a.Regs[hint] == 0 {
		a.Regs[hint] = 1
		return hint
	}

	for r := a.Args; r < len(a.Regs); r++ {
		if a.Regs[r] == 0 {
			a.Regs[r] = 1
			return r
		}
	}

	panic("no more")
}

func (a *Arch) Free(reg int) {
	tlog.Printw("free register", "reg", reg, "from", loc.Callers(1, 3))

	if a.Regs[reg] == 0 {
		panic("double free")
	}

	a.Regs[reg] = 0
}
