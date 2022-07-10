package ast

type (
	Node interface {
	}

	Base struct {
		Pos int
		End int
	}

	LineBreak struct {
		Base
	}

	Ident struct {
		Base `tlog:",embed"`
	}

	Token struct {
		Base `tlog:",embed"`
	}

	Int struct {
		Base `tlog:",embed"`
	}

	Float struct {
		Base `tlog:",embed"`
	}

	Add struct {
		Base `tlog:",embed"`

		Left  Node
		Right Node
	}
)
