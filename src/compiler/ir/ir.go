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

		Exprs  []any
		Blocks []Block
	}

	Block struct {
		Phi  []Expr
		Code []Expr

		Next int
	}

	Expr int

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

	//	B struct {
	//		Block int
	//	}

	BCond struct {
		Expr  Expr
		Cond  Cond
		Block int
	}

	Phi []Expr
)
