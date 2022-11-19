package ir

type (
	Word uint64
	Expr int
	Cond int

	Param struct{}

	Phi struct {
		Cond Cond
		Expr Expr
		Then Expr
		Else Expr
	}

	Add struct {
		Left  Expr
		Right Expr
	}

	Cmp struct {
		Left  Expr
		Right Expr
	}

	Func struct {
		Name string
		In   []Param
		Out  []Param
		//	Body Expr

		Exprs []any
	}

	Package struct {
		Path string

		Funcs []*Func
	}
)
