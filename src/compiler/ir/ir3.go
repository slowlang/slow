//go:build ignore

package ir

import "github.com/slowlang/slow/src/compiler/tp"

type (
	Package struct {
		Path string

		Types []*tp.Spec
		Funcs []*Func

		Exprs []any
	}

	Func struct {
		Name string

		In  []Param
		Out []Expr

		Code []Expr

		ABI ABI
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

	Data string

	Zero struct{}

	ABI int

	Ptr struct {
		X Expr
	}

	Struct []Expr

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

	Index struct {
		X, I Expr
	}

	Field struct {
		X Expr
		I int
	}

	Call struct {
		Func string

		In []Expr

		ABI ABI
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
