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
	}

	Func struct {
		Name    string
		Args    BlockID
		Results []Link
	}

	Block interface {
		//	In() []Link
		//	Out() []Link
	}

	Tuple []Link

	Type    int
	BlockID int
	Cond    string
	Imm     int64
	Zero    struct{}
	Args    struct{}

	Link struct {
		Block BlockID
		Out   int
	}

	Call struct {
		Func    Link
		Args    []Link
		Context []Link
	}

	Pred struct {
		Expr Link
		Cond Cond
	}

	Switch struct {
		Preds   []Pred
		Blocks  []BlockID
		Context []Link
	}

	Loop struct {
		Cond    Pred
		Body    BlockID
		Context []Link
	}

	Cmp struct {
		L, R Link
	}

	Add struct {
		L, R Link
	}

	Sub struct {
		L, R Link
	}

	Mul struct {
		L, R Link
	}
)

var Nowhere = Link{Block: -1}

func (l Link) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	if l == Nowhere {
		return e.AppendNil(b)
	}

	if l.Out == 0 {
		return e.AppendFormat(b, "%d", l.Block)
	}

	return e.AppendFormat(b, "%d_%d", l.Block, l.Out)
}
