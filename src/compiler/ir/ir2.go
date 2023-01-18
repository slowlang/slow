package ir

import (
	"github.com/nikandfor/tlog/tlwire"
)

type (
	Package struct {
		Path string

		Self  BlockID
		Funcs []BlockID

		Blocks []Block `tlog:"-"`
		Regs   []Link  `tlog:"-"`
	}

	Func struct {
		Name    string
		Args    BlockID
		Results []Reg
	}

	Block interface {
		In() []Reg
	}

	Tuple []Reg

	Args    int
	BlockID int
	Cond    string
	Imm     int64
	Reg     int
	Type    int
	Zero    struct{}

	Link struct {
		Block BlockID
		Out   int
	}

	Call struct {
		Func    Reg
		Args    []Reg
		Context []Reg
	}

	Pred struct {
		Expr Reg
		Cond Cond
	}

	Switch struct {
		Preds   []Pred
		Blocks  []BlockID
		Context []Reg
	}

	Loop struct {
		Cond    Pred
		Body    BlockID
		LoopIn  []Reg
		LoopOut []Reg
		Context []Reg
	}

	Cmp struct {
		L, R Reg
	}

	Add struct {
		L, R Reg
	}

	Sub struct {
		L, R Reg
	}

	Mul struct {
		L, R Reg
	}
)

var Nowhere Reg = -1

func (x Zero) In() []Reg    { return nil }
func (x Args) In() []Reg    { return nil }
func (x Imm) In() []Reg     { return nil }
func (x Package) In() []Reg { return nil }
func (x Func) In() []Reg    { return nil }

func (x Tuple) In() []Reg { return x }

func (x Cmp) In() []Reg { return []Reg{x.L, x.R} }
func (x Add) In() []Reg { return []Reg{x.L, x.R} }
func (x Sub) In() []Reg { return []Reg{x.L, x.R} }
func (x Mul) In() []Reg { return []Reg{x.L, x.R} }

func (x *Switch) In() []Reg {
	var l []Reg

	for _, p := range x.Preds {
		l = append(l, p.Expr)
	}

	l = append(l, x.Context...)

	return l
}

func (x *Loop) In() []Reg {
	var l []Reg

	l = append(l, x.Cond.Expr)
	l = append(l, x.LoopIn...)
	l = append(l, x.Context...)

	return l
}

func (x Call) In() []Reg {
	var l []Reg

	l = append(l, x.Args...)
	l = append(l, x.Context...)

	return l
}

func (l Link) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	if l.Block == -1 {
		return e.AppendNil(b)
	}

	if l.Out == 0 {
		return e.AppendFormat(b, "%d", l.Block)
	}

	return e.AppendFormat(b, "%d_%d", l.Block, l.Out)
}
