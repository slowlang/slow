package ir

type (
	Word uint64
	Expr int
	Arg  int
	Cond string

	Param struct {
		Name string
		Expr Expr
	}

	Phi []Expr

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

		Exprs  []any
		Blocks []Block
	}

	Block struct {
		In  []Expr
		Out []Expr

		Ops []any

		Next int
	}

	Package struct {
		Path string

		Funcs []*Func
	}

	//	Branch struct {
	//		Block int
	//	}

	BranchIf struct {
		Cond  Cond
		Expr  Expr
		Block int
	}
)
