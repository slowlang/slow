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
		Held bool
	}

	RawBlock struct {
		In  []Expr
		Out []Expr

		Code []Expr
	}

	Merge []MergeOut

	MergeOut struct {
		Expr  Expr
		Preds []Pred
	}
)
