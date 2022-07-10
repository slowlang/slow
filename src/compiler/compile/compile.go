package compile

import (
	"context"

	"github.com/nikandfor/errors"

	"github.com/nikandfor/hacked/hfmt"
	"github.com/slowlang/slow/src/compiler/ir"
)

func Compile(ctx context.Context, y ir.Node) (obj []byte, err error) {
	obj = append(obj, `
.align 4
.global _start
_start:
	MOV	FP, SP
	SUB	SP, SP, #16
	STR	LR, [FP, #-8]

`...)

	obj, err = compile(ctx, obj, 0, y)
	if err != nil {
		return nil, err
	}

	obj = append(obj, `
	LDR	LR, [FP, #-8]
	ADD	SP, SP, #16
	RET
`...)

	return obj, nil
}

func compile(ctx context.Context, b []byte, reg int, y ir.Node) (_ []byte, err error) {
	switch y := y.(type) {
	case ir.Word:
		b = movConst(ctx, b, reg, y)
	case ir.Sum:
		if len(y) == 0 {
			break
		}

		b, err = compile(ctx, b, reg, y[0])
		if err != nil {
			return b, errors.Wrap(err, "sum")
		}

		for i := 1; i < len(y); i++ {
			b, err = compile(ctx, b, reg+1, y[i])
			if err != nil {
				return b, errors.Wrap(err, "sum")
			}

			b = hfmt.AppendPrintf(b, "	ADD	X%d, X%[1]d, X%d\n", reg, reg+1)
		}
	default:
		return nil, errors.New("unsupported node: %T", y)
	}

	return b, nil
}

func movConst(ctx context.Context, b []byte, reg int, y ir.Word) []byte {
	b = hfmt.AppendPrintf(b, "	MOV	X%v, #%d\n", reg, y.Value&0xffff)

	for sh := 0; y.Value > 0xffff; sh += 16 {
		y.Value >>= 16

		b = hfmt.AppendPrintf(b, "	MOV	X%v, #%d, LSL #%d // Word\n", reg, y.Value&0xffff, sh)
	}

	return b
}
