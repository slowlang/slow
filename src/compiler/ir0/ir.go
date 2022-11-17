package ir

type (
	Package struct {
		Path string
		Name string

		Funcs []*Func
	}

	Func struct {
		Name string
		Args []*Type
		Ret  []int

		Exprs  []Expr
		Blocks []Block
	}

	Block struct {
		Next int

		Stmts []Stmt
	}

	Arg struct {
		Num int
	}

	Type struct {
		Size int
	}

	Stmt interface{}

	Expr struct {
		Value any

		UsedBy []int
	}

	Ret struct {
		Value int
	}

	If struct {
		Cond int
		Then int
	}

	For struct {
		Cond int
		Body int
	}

	BinOp struct {
		Op    string
		Left  int
		Right int
	}
)
