package compile

import (
	"context"

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

	switch y := y.(type) {
	case ir.Word:
		obj = hfmt.AppendPrintf(obj, "\n	MOV	X0, #%d // Word\n", y.Value&0xffff)

		for sh := 0; y.Value > 0xffff; sh += 16 {
			y.Value >>= 16

			obj = hfmt.AppendPrintf(obj, "\n	MOV	X0, #%d, LSL #%d // Word\n", y.Value&0xffff, sh)
		}
	default:
		obj = append(obj, `
	MOV X0, #0 // default
`...)
	}

	obj = append(obj, `
	LDR	LR, [FP, #-8]
	ADD	SP, SP, #16
	RET
`...)

	return obj, nil
}
