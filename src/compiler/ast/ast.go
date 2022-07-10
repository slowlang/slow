package ast

type (
	Node interface {
	}

	Base struct {
		Pos int
	}

	LineBreak struct {
		Base
	}

	Ident struct {
		Base `tlog:",embed"`

		Name string
	}

	Int struct {
		Base `tlog:",embed"`

		End int
	}
)
