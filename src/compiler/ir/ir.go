package ir

type (
	Word uint64
	Expr int
	Arg  int
	Cond string

	Param struct {
		Expr Expr
	}

	Phi struct {
		Cond Expr
		Then Expr
		Else Expr
	}

	Add struct {
		Left  Expr
		Right Expr
	}

	Cmp struct {
		Cond  Cond
		Left  Expr
		Right Expr
	}

	Func struct {
		Name string
		In   []Param
		Out  []Param

		Exprs []any
	}

	Package struct {
		Path string

		Funcs []*Func
	}
)
