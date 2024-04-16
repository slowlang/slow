package df

import "github.com/slowlang/slow/src/compiler/ir"

type (
	Expr = ir.Expr
	Out  = ir.Out

	Cond = ir.Cond

	Alias ir.Expr

	Tuple []Expr

	Pred struct {
		Expr Expr
		Cond Cond
	}

	Block struct {
		In  []Expr
		Out []Expr

		Code []Expr
	}

	Break struct{}
	Loop  struct{}
)
