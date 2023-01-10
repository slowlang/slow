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

		Entry int

		Exprs  []any
		Blocks []Block
	}

	Block struct {
		Phi  []Expr
		Code []Expr

		Loop int
	}

	Expr int

	Cond string

	Param struct {
		Name string
		Expr Expr
	}

	Arg int

	Imm int64

	Add struct {
		L, R Expr
	}

	Cmp struct {
		L, R Expr
	}

	B struct {
		Block int
	}

	BCond struct {
		Expr  Expr
		Cond  Cond
		Block int
	}

	Phi []Expr
)

type (
	Iner interface {
		In() []Expr
	}
)

func (x Add) In() []Expr {
	return []Expr{x.L, x.R}
}

func (x Cmp) In() []Expr {
	return []Expr{x.L, x.R}
}

func (x BCond) In() []Expr {
	return []Expr{x.Expr}
}
