package ir

import "tlog.app/go/tlog/tlwire"

type (
	Expr int
	Type Expr

	Args int
	Out  Expr
	Zero struct{}

	Label int
	Cond  string

	//	State  Expr
	//	Effect Expr

	Phi []PhiBranch

	PhiBranch struct {
		B    Expr
		Expr Expr
	}

	Imm int64

	Package struct {
		Path string

		Funcs []Expr

		Exprs []any
		EType []Type
	}

	Func struct {
		Name string

		In  []Expr
		Out []Expr

		//	State struct {
		//		In, Out State `tlog:",omitempty"`
		//	}
		//	Effect struct {
		//		In, Out Effect `tlog:",omitempty"`
		//	}

		Code []Expr
	}

	Call struct {
		Func Expr

		In []Expr

		//	StateIn  State  `tlog:",omitempty"`
		//	EffectIn Effect `tlog:",omitempty"`
	}

	B struct {
		Label Label
	}

	BCond struct {
		Expr  Expr
		Label Label
	}

	Assert struct {
		Expr Expr
	}

	//

	Cmp struct {
		L, R Expr
		Cond Cond
	}

	Add struct {
		L, R Expr
	}

	Sub struct {
		L, R Expr
	}

	Mul struct {
		L, R Expr
	}

	Div struct {
		L, R Expr
	}

	Mod struct {
		L, R Expr
	}

	LogicAnd struct {
		L, R Expr
	}

	LogicOr struct {
		L, R Expr
	}

	BitAnd struct {
		L, R Expr
	}

	BitOr struct {
		L, R Expr
	}

	LogicShiftLeft struct {
		L, R Expr
	}

	LogicShiftRight struct {
		L, R Expr
	}

	ArithShiftRight struct {
		L, R Expr
	}

	RotateRight struct {
		L, R Expr
	}
)

const (
	Nil Expr = -1
)

const (
	Eq = iota
	Ne
	Lt
	Ge
	Le
	Gt
	Bz
	Bn
)

func (p PhiBranch) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	b = e.AppendMap(b, 2)
	b = e.AppendKeyInt64(b, "b", int64(p.B))
	b = e.AppendKeyInt64(b, "id", int64(p.Expr))

	return b
}
