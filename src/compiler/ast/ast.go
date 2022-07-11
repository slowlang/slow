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

	Type struct {
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

	VarDecl struct {
		Base `tlog:",embed"`
		Name Ident
		Type Type
	}

	Assignment struct {
		Base  `tlog:",embed"`
		Left  Ident
		Right Node
	}
)
