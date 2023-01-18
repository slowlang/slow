package ir

type (
	Package struct {
		Path string

		Funcs []*Func

		Exprs []any
	}

	Func struct {
		Name string

		In  []Param
		Out []Expr

		Code []Expr
	}

	Param struct {
		Name string
		Expr Expr
	}

	Args int
	Cond string
	Expr int
	Imm  int64
	Out  int

	Zero struct{}

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

	Call struct {
		Func string

		In []Expr
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

	Nop struct{}
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
