package ir

import "github.com/nikandfor/tlog/wire"

type (
	Package struct {
		Funcs []Func
	}

	Func struct {
		Name string

		In  []Arg
		Out []Arg
	}

	Arg struct {
		Num int
	}

	Var struct {
		Name string
		ID   int
	}

	Assignment struct {
		Lhs Expr
		Rhs Expr
	}

	BinOp struct {
		Op    string
		Left  Expr
		Right Expr
	}

	Phi struct {
		Cond Expr
		Then Expr
		Else Expr
	}

	Return struct {
		Value Expr
	}

	Expr interface{}

	Word struct {
		Value int64
	}
)

func (v Var) TlogAppend(e *wire.Encoder, b []byte) []byte {
	return e.AppendFormat(b, "%v_%d", v.Name, v.ID)
}
