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
