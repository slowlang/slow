package back

import (
	"context"
	"fmt"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"

	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Arch interface {
		Alloc(e ir.Expr) int
		Free(reg int)
	}

	Compiler struct{}

	state struct {
		f *ir.Func

		deps map[int][]int

		//	hint     map[ir.Expr]int
		compiled map[int]struct{}

		reg int
	}
)

func New() *Compiler { return nil }

func (c *Compiler) CompilePackage(ctx context.Context, a Arch, b []byte, p *ir.Package) (_ []byte, err error) {
	b = fmt.Appendf(b, `// package %s

.global _start
.align 4
_start:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP

	BL	main

	LDP     FP, LR, [SP], #16
	RET
`, p.Path)

	for _, f := range p.Funcs {
		b = append(b, '\n')

		b, err = c.compileFunc(ctx, a, b, f)
		if err != nil {
			return nil, errors.Wrap(err, "func %v", f.Name)
		}
	}

	return b, nil
}

func (c *Compiler) compileFunc(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	b = fmt.Appendf(b, `.global %s
.align 4
%[1]s:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP
`, f.Name)

	s := &state{
		f:    f,
		deps: map[int][]int{},
		//	hint:     map[ir.Expr]int{},
		compiled: map[int]struct{}{},
		reg:      len(f.In),
	}

	for id := range f.Blocks {
		c.calcDeps(ctx, a, s, id)
	}

	tlog.Printw("block deps", "block_deps", s.deps)

	//	for i, p := range f.Out {
	//		s.hint[p.Expr] = i
	//	}

	b, err = c.compileBlock(ctx, a, b, s, 0)
	if err != nil {
		return nil, errors.Wrap(err, "")
	}

	b = fmt.Appendf(b, `
ret_%s:
	LDP     FP, LR, [SP], #16
	RET
`, f.Name)

	return b, nil
}

func (c *Compiler) calcDeps(ctx context.Context, a Arch, s *state, block int) {
	bp := &s.f.Blocks[block]

	if bp.Next >= 0 {
		s.addDep(bp.Next, block)
	}

	for _, op := range bp.Ops {
		switch op := op.(type) {
		//	case ir.Branch:
		//		s.addDep(op.Block, block)
		case ir.BranchIf:
			s.addDep(op.Block, block)
		}
	}
}

func (c *Compiler) compileBlock(ctx context.Context, a Arch, b []byte, s *state, block int) (_ []byte, err error) {
	if _, ok := s.compiled[block]; ok {
		return b, nil
	}

	s.compiled[block] = struct{}{}

	buf, err := c.compileBlockData(ctx, a, nil, s, block)
	if err != nil {
		return nil, errors.Wrap(err, "")
	}

	for _, d := range s.deps[block] {
		b, err = c.compileBlock(ctx, a, b, s, d)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}
	}

	b = append(b, buf...)

	return b, nil
}

func (c *Compiler) compileBlockData(ctx context.Context, a Arch, b []byte, s *state, block int) (_ []byte, err error) {
	bp := &s.f.Blocks[block]

	b = fmt.Appendf(b, "\nblock_%d:\n", block)

	for _, e := range bp.Out {
		_, b, err = c.compileExpr(ctx, a, b, s, e, block)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}
	}

	for _, op := range bp.Ops {
		switch op := op.(type) {
		case ir.BranchIf:
			_, b, err = c.compileExpr(ctx, a, b, s, op.Expr, block)
			if err != nil {
				return
			}

			cond := ""

			switch op.Cond {
			case "<":
				cond = "LS"
			case ">":
				cond = "GT"
			default:
				return nil, errors.New("unsupported operation: %q", op.Cond)
			}

			b = fmt.Appendf(b, "	B.%s	block_%d\n", cond, op.Block)
		default:
			return nil, errors.New("unsupported op: %T", op)
		}
	}

	if bp.Next >= 0 {
		b = fmt.Appendf(b, "	B	block_%d\n", bp.Next)
	}

	return b, nil
}

func (c *Compiler) compileExpr(ctx context.Context, a Arch, b []byte, s *state, e ir.Expr, block int) (reg int, _ []byte, err error) {
	//	b = fmt.Appendf(b, "	// X%d <- expr %d  %T%[3]v\n", reg, e, f.Exprs[e])

	reg = s.reg
	s.reg++

	tlog.Printw("alloc reg", "reg", reg, "expr", e, "block", block)

	bp := &s.f.Blocks[block]
	if exprIn(e, bp.In) {
		return reg, b, nil
	}

	//	reg := a.Alloc(e)

	switch x := s.f.Exprs[e].(type) {
	case ir.Word:
		b = fmt.Appendf(b, "	MOV	X%d, #%d	// expr %d\n", reg, x, e)
	case ir.Arg:
		if int(x) != reg {
			b = fmt.Appendf(b, "	MOV	X%d, X%d	// arg %d\n", reg, x, x)
		} else {
			b = fmt.Appendf(b, "	// arg %d is already in place\n", x)
		}
	case ir.Phi:
		b = fmt.Appendf(b, "	// X%d is %v	// expr %d\n", reg, x, e)
	//	for _, e := range x {
	//		s.hint[e] = reg
	//	}
	case ir.Cmp:
		var lr, rr int

		lr, b, err = c.compileExpr(ctx, a, b, s, x.Left, block) //, reg+1)
		if err != nil {
			return
		}

		rr, b, err = c.compileExpr(ctx, a, b, s, x.Right, block) //, reg+2)
		if err != nil {
			return
		}

		b = fmt.Appendf(b, "	CMP	X%d, X%d\n", lr, rr)
	case ir.Add:
		var lr, rr int

		lr, b, err = c.compileExpr(ctx, a, b, s, x.Left, block) //, reg)
		if err != nil {
			return
		}

		rr, b, err = c.compileExpr(ctx, a, b, s, x.Right, block) //, reg+1)
		if err != nil {
			return
		}

		b = fmt.Appendf(b, "	ADD	X%d, X%d, X%d	// expr %d\n", reg, lr, rr, e)
	default:
		return 0, nil, errors.New("unsupported expr: %T", x)
	}

	return reg, b, nil
}

func (s *state) addDep(b, dep int) {
	for _, d := range s.deps[b] {
		if dep == d {
			return
		}
	}

	s.deps[b] = append(s.deps[b], dep)
}

func exprIn(e ir.Expr, in []ir.Expr) bool {
	for _, x := range in {
		if e == x {
			return true
		}
	}

	return false
}
