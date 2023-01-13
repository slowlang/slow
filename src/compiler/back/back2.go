package back

import (
	"context"
	"fmt"
	"math/bits"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/nikandfor/tlog/tlwire"

	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Arch interface{}

	Compiler struct {
	}

	Set uint64
)

func New() *Compiler {
	return nil
}

func (c *Compiler) CompilePackage(ctx context.Context, a Arch, b []byte, p *ir.Package) (_ []byte, err error) {
	b = fmt.Appendf(b, `// package %s

.global _start
.align 4
_start:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP

	BL	_main

	LDP     FP, LR, [SP], #16
	RET
`, p.Path)

	for _, f := range p.Funcs {
		b = append(b, '\n')

		b, err = c.compileFunc(ctx, a, b, f)
		if err != nil {
			return nil, errors.Wrap(err, "func %v", f.Name)
		}
	}

	return b, nil
}

func (c *Compiler) compileFunc(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	i2b := make([]int, len(f.Code))
	l2i := make([]ir.Expr, 0, 10)
	b2i := make([]ir.Expr, 0, 10)

	setLabel := func(l ir.Label, bb int, id ir.Expr) {
		for int(l) >= len(l2i) {
			l2i = append(l2i, -1)
		}

		l2i[l] = id

		for bb >= len(b2i) {
			b2i = append(b2i, -1)
		}

		b2i[bb] = id
	}

	bb := 0
	next := false

	for id, x := range f.Code {
		if l, ok := x.(ir.Label); ok || next {
			bb++
			next = false

			setLabel(l, bb, ir.Expr(id))
		}

		i2b[id] = bb

		//	tlog.Printw("func code", "bb", bb, "id", id, "typ", tlog.FormatNext("%T"), x, "val", x)

		if _, ok := x.(ir.B); ok {
			next = true
		}
		if _, ok := x.(ir.BCond); ok {
			next = true
		}
	}

	loop := make([]int, len(f.Code))

	for id, x := range f.Code {
		l := ir.Label(-1)

		if br, ok := x.(ir.B); ok {
			l = br.Label
		}
		if br, ok := x.(ir.BCond); ok {
			l = br.Label
		}

		if l < 0 {
			continue
		}

		for j := l2i[l]; j <= ir.Expr(id); j++ {
			loop[j]++
		}
	}

	slots := make([]Set, len(f.Code))

	if true { // backward
		var d Set

		for _, p := range f.Out {
			d.Add(p.Expr)
		}

		labelneed := make([]Set, len(l2i))

		for run := 0; run < 2; run++ {
			d.Reset()

			for id := ir.Expr(len(f.Code)) - 1; id >= 0; id-- {
				x := f.Code[id]

				switch x := x.(type) {
				case ir.B:
					d = labelneed[x.Label]
				case ir.BCond:
					d.Or(labelneed[x.Label])
				}

				if tlog.If("slot_pass") {
					switch x.(type) {
					case ir.Label, ir.B, ir.BCond:
						tlog.Printw("backward pass", "run", run, "id", id, "slots", d)
					}
				}

				switch x.(type) {
				case ir.Label, ir.Phi:
				default:
					slots[id] = d
				}

				switch x := x.(type) {
				case ir.Imm, ir.Arg:
				case ir.Label:
					labelneed[x] = d
				case ir.Phi:
					for _, xx := range x {
						d.Add(xx)
					}
				case ir.B:
				case ir.BCond:
					d.Add(x.Expr)
				case ir.Cmp:
					d.Add(x.L, x.R)
				case ir.Add:
					d.Add(x.L, x.R)
				case ir.Sub:
					d.Add(x.L, x.R)
				case ir.Mul:
					d.Add(x.L, x.R)
				default:
					panic(x)
				}

				d.Del(id)
			}
		}

		if tlog.If("labelneed") {
			for l, d := range labelneed {
				tlog.Printw("labelneed", "label", l, "need", d)
			}
		}
	}

	if true { // forward
		var d Set

		for _, p := range f.In {
			d.Add(p.Expr)
		}

		labelhave := make([]Set, len(l2i))

		for run := 0; run < 2; run++ {
			d.Reset()

			check := func(ids ...ir.Expr) {
				if run == 0 {
					return
				}

				for _, id := range ids {
					if !d.IsSet(id) {
						panic(id)
					}
				}
			}

			for id, x := range f.Code {
				d.Add(ir.Expr(id))

				switch x := x.(type) {
				case ir.Imm, ir.Arg:
				case ir.Label:
					d.Or(labelhave[x])
				case ir.B:
					labelhave[x.Label].Or(d)
				case ir.BCond:
					check(x.Expr)
					labelhave[x.Label].Or(d)
				case ir.Phi:
					for _, xx := range x {
						check(xx)
						d.Del(xx)
					}
				case ir.Cmp:
					check(x.L, x.R)
				case ir.Add:
					check(x.L, x.R)
				case ir.Sub:
					check(x.L, x.R)
				case ir.Mul:
					check(x.L, x.R)
				default:
					panic(x)
				}

				if tlog.If("slot_pass") {
					switch x.(type) {
					case ir.Label, ir.B, ir.BCond:
						tlog.Printw("forward pass", "run", run, "id", id, "slots", d)
					}
				}

				if run != 0 {
					slots[id].And(d)
				}

				switch x.(type) {
				case ir.B:
					d.Reset()
				}
			}
		}

		if tlog.If("labelhave") {
			for l, d := range labelhave {
				tlog.Printw("labelhave", "label", l, "have", d)
			}
		}
	}

	{ // phi
		var d Set

		for id := ir.Expr(len(f.Code)) - 1; id >= 0; id-- {
			x := f.Code[id]

			switch x.(type) {
			case ir.Label:
				slots[id] = d
				d.Reset()
			case ir.Phi:
				d.Add(id)
			}
		}
	}

	graph := make([]Set, len(f.Code))
	clique := 0

	for id := range f.Code {
		if s := slots[id].Size(); s > clique {
			clique = s
		}

		slots[id].Range(func(x ir.Expr) {
			slots[id].Range(func(y ir.Expr) {
				if x == y {
					return
				}

				//	if _, ok := f.Code[x].(ir.Phi); ok {
				//		return
				//	}

				graph[x].Add(y)
				graph[y].Add(x)
			})
		})
	}

	tlog.Printw("graph", "clique", clique)

	for id, d := range graph {
		if d.Size() == 0 {
			continue
		}

		tlog.Printw("graph", "id", id, "x", d)
	}

	for id, x := range f.Code {
		if phi, ok := x.(ir.Phi); ok {
			tlog.Printw("phi", "id", id, "phi", phi)
		}
	}

	// TODO: spill if len(registers) < clique

	tlog.Printw("compile func", "name", f.Name, "in", f.In, "out", f.Out)

	for id, x := range f.Code {
		tlog.Printw("func code", "bb", i2b[id], "loop", loop[id], "id", id, "typ", tlog.FormatNext("%T"), x, "val", x, "slots", slots[id])
	}

	return b, nil
}

func (d *Set) Add(ids ...ir.Expr) {
	for _, id := range ids {
		*d |= 1 << id
	}
}

func (d *Set) Del(ids ...ir.Expr) {
	for _, id := range ids {
		*d &^= 1 << id
	}
}

func (d *Set) Or(x Set) {
	*d |= x
}

func (d *Set) And(x Set) {
	*d &= x
}

func (d *Set) Reset() {
	*d = 0
}

func (d Set) Size() (s int) {
	return bits.OnesCount64(uint64(d))
}

func (d Set) IsSet(id ir.Expr) bool {
	return d&(1<<id) != 0
}

func (d Set) Range(f func(id ir.Expr)) {
	for i := 0; i < 64; i++ {
		if d&(1<<i) != 0 {
			f(ir.Expr(i))
		}
	}
}

func (d Set) TlogAppend(b []byte) []byte {
	if d.Size() == 0 {
		return (tlwire.LowEncoder{}).AppendSpecial(b, tlwire.None)
	}

	b = (tlwire.LowEncoder{}).AppendTag(b, tlwire.Array, -1)

	for i := 0; i < 64; i++ {
		if d&(1<<i) != 0 {
			b = (tlwire.LowEncoder{}).AppendInt(b, i)
		}
	}

	b = (tlwire.LowEncoder{}).AppendSpecial(b, tlwire.Break)

	return b
}
