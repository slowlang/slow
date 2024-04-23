package df

import (
	"github.com/slowlang/slow/src/compiler/ir"
	"tlog.app/go/tlog/tlwire"
)

type (
	Expr = ir.Expr
	Out  = ir.Out

	Alias ir.Expr

	Tuple []Expr

	RawBlock struct {
		In  []Expr
		Out []Expr

		Code []Expr
	}

	Pred Expr

	Merge []MergeOpt

	MergeOpt struct {
		Expr Expr
		Pred []Pred
	}
)

const (
	PredHeld = 1 << iota
	PredNotHeld
)

func ToPred(e Expr, held int) Pred {
	return Pred(e<<2) | Pred(held&3)
}

func (p Pred) Expr() Expr {
	return Expr(p >> 2)
}

func (p Pred) Held() int {
	return int(p & 3)
}

func (p Pred) TlogAppend(buf []byte) []byte {
	var e tlwire.Encoder

	var c byte

	switch p.Held() {
	case PredHeld:
		c = '>'
	case PredNotHeld:
		c = 'v'
	case PredHeld | PredNotHeld:
		c = '&'
	case 0:
		c = '_'
	default:
		c = '?'
	}

	return e.AppendFormat(buf, "%d%c", p.Expr(), c)
}
