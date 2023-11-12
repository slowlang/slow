package df

import "github.com/slowlang/slow/src/compiler/ir"

type (
	Expr = ir.Expr
	Out  = ir.Out

	Alias ir.Expr

	Cond = ir.Cond

	Tuple []Expr

	RawBlock struct {
		In  []Expr
		Out []Expr

		Code []Expr
	}

	Pred struct {
		Expr Expr
		Cond Cond
	}

	Switch struct {
		Preds    []Pred
		Branches []Tuple // for each pred + else
	}

	Loop struct {
	}
)
