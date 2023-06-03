//go:build ignore

package ir

type (
	Expr int
	Type int

	Args int
	Cond string
	//	Data  []byte
	Imm   int64
	Label int
	Out   Expr
	Zero  struct{}

	State Expr

	ABI struct{}

	Package struct {
		Path string

		Funcs []Expr

		Exprs []any  `tlog:"-"`
		EType []Type `tlog:"-"`
	}

	Func struct {
		Name string

		In  []Expr
		Out []Expr

		StateIn, StateOut State

		Code []Expr
	}

	Call struct {
		Func Expr
		Args []Expr
	}

	B struct {
		Label Label
	}

	BCond struct {
		Expr  Expr
		Cond  Cond
		Label Label
	}

	Phi []Expr

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

	Ptr struct {
		X Expr
	}

	Offset struct { // result is pointer
		Base   Expr // pointer
		Offset Expr
		Size   Expr
	}

	Alloc struct {
		Type Type
	}

	Store struct {
		Ptr   Expr
		Val   Expr
		State State
	}

	Load struct {
		Ptr   Expr
		State State
	}
)
