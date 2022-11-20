package back

import (
	"context"
	"fmt"

	"github.com/nikandfor/errors"

	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Arch interface {
	}

	Compiler struct{}

	Expr any
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

	for i, p := range f.Out {
		b, err = c.compileExpr(ctx, a, b, f, p.Expr, len(f.In)+i, nil)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}
	}

	b = append(b, "\n"...)

	for i := range f.Out {
		b = fmt.Appendf(b, "	MOV	X%d, X%d	// move result\n", i, len(f.In)+i)
	}

	b = fmt.Appendf(b, `
ret_%s:
	LDP     FP, LR, [SP], #16
	RET
`, f.Name)

	return b, nil
}

func (c *Compiler) compileExpr(ctx context.Context, a Arch, b []byte, f *ir.Func, e ir.Expr, reg int, cond *string) (_ []byte, err error) {
	//	b = fmt.Appendf(b, "	// X%d <- expr %d  %T%[3]v\n", reg, e, f.Exprs[e])

	switch x := f.Exprs[e].(type) {
	case ir.Word:
		b = fmt.Appendf(b, "	MOV	X%d, #%d\n", reg, x)
	case ir.Arg:
		if int(x) != reg {
			b = fmt.Appendf(b, "	MOV	X%d, X%d	// arg %d\n", reg, x, x)
		} else {
			b = fmt.Appendf(b, "	// arg %d is already in place\n", x)
		}
	case ir.Phi:
		b = fmt.Appendf(b, "	// if_%d\n", e)

		var cond string

		b, err = c.compileExpr(ctx, a, b, f, x.Cond, reg, &cond)
		if err != nil {
			return
		}

		b = fmt.Appendf(b, "	B.%s	then_%d\n", cond, e)
		b = fmt.Appendf(b, "	B	else_%d\n", e)

		b = fmt.Appendf(b, "then_%d:\n", e)

		b, err = c.compileExpr(ctx, a, b, f, x.Then, reg, nil)
		if err != nil {
			return
		}

		b = fmt.Appendf(b, "	B	endif_%d\n", e)

		b = fmt.Appendf(b, "else_%d:\n", e)

		b, err = c.compileExpr(ctx, a, b, f, x.Else, reg, nil)
		if err != nil {
			return
		}

		b = fmt.Appendf(b, "endif_%d:\n", e)
	case ir.Cmp:
		b, err = c.compileExpr(ctx, a, b, f, x.Left, reg+1, nil)
		if err != nil {
			return
		}

		b, err = c.compileExpr(ctx, a, b, f, x.Right, reg+2, nil)
		if err != nil {
			return
		}

		switch x.Cond {
		case "<":
			*cond = "LS"
		case ">":
			*cond = "GT"
		default:
			return nil, errors.New("unsupported operation: %q", x.Cond)
		}

		b = fmt.Appendf(b, "	CMP	X%d, X%d\n", reg+1, reg+2)
	case ir.Add:
		b, err = c.compileExpr(ctx, a, b, f, x.Left, reg, nil)
		if err != nil {
			return
		}

		b, err = c.compileExpr(ctx, a, b, f, x.Right, reg+1, nil)
		if err != nil {
			return
		}

		b = fmt.Appendf(b, "	ADD	X%d, X%d, X%d	// expr %d\n", reg, reg, reg+1, e)
	default:
		return nil, errors.New("unsupported expr: %T", x)
	}

	return b, nil
}
