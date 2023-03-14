package tp

import "github.com/slowlang/slow/src/compiler/ir"

type (
	Name string
	Type = ir.Type

	Func struct {
		In  []Type
		Out []Type
	}

	Int struct {
		Bits   int16
		Signed bool
	}

	Untyped struct{}

	Ptr struct {
		X Type
	}

	Array struct {
		Elem Type
		Len  ir.Expr
	}

	Struct struct {
		Fields []StructField
	}

	StructField struct {
		Name   string
		Offset int
		Type   Type
	}
)

func (x Int) Size() int {
	return int(x.Bits) / 8
}
