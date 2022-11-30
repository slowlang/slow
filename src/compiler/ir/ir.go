package ir

type (
	Package struct {
		Path string

		Funcs []*Func
	}

	Func struct {
		Name string

		In  []Param
		Out []Param

		Code []any
	}

	Expr int

	Label int

	Cond string

	Param struct {
		Name string
		Expr Expr
	}

	Arg int

	Word uint64

	Add struct {
		L, R Expr
	}

	Cmp struct {
		L, R Expr
	}

	B struct {
		Label Label
	}

	BCond struct {
		Expr  Expr
		Cond  Cond
		Label Label
	}

	Phi []Expr
)
