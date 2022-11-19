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
	BL	main
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

func (c *Compiler) compileFunc(ctx context.Context, a Arch, b []byte, f *ir.Func) ([]byte, error) {
	b = fmt.Appendf(b, `.global %s
.align 4
%[1]s:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP

`, f.Name)

	b = append(b, `
prologue:
        LDP     FP, LR, [SP], #16
        RET
`...)

	return b, nil
}
