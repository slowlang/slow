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

	Sub struct {
		L, R Expr
	}

	Mul struct {
		L, R Expr
	}

	Cmp struct {
		L, R Expr
	}

	Label int

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
