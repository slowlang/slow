package tp

type (
	Type interface {
		Size() int
	}

	Name string

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
		X   Type
		Len int
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

func (x Ptr) Size() int {
	return 8
}

func (x Array) Size() int {
	return x.X.Size() * x.Len
}

func (x Struct) Size() (s int) {
	for _, f := range x.Fields {
		s += f.Type.Size()
	}

	return s
}
