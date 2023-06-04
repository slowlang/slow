package tp

import "github.com/slowlang/slow/src/compiler/ir"

type (
	Type = ir.Type

	Definition interface {
		MemAlign() int
		MemSize() int
	}

	Bool    struct{}
	Cmp     struct{}
	TypeDef struct{}
	State   struct{}
	Unit    struct{}
	Untyped struct{}

	Func struct {
		In  []Type
		Out []Type
	}

	Int struct {
		Bits   int16
		Signed bool
	}

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

func (x Int) MemAlign() int {
	return x.MemSize()
}

func (x Int) MemSize() int {
	if x.Bits == 0 {
		return 8
	}

	return int(x.Bits) / 8
}
