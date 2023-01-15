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

	Context struct {
		a Arch
		p *ir.Package
	}

	Set uint64
)

func New() *Compiler {
	return nil
}

func (c *Compiler) CompilePackage(ctx context.Context, a Arch, b []byte, p *ir.Package) (_ []byte, err error) {
	cc, err := c.prepare(ctx, a, p)
	if err != nil {
		return nil, errors.Wrap(err, "prepare")
	}

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

		b, err = c.compileFunc(ctx, b, cc, f)
		if err != nil {
			return nil, errors.Wrap(err, "func %v", f.Name)
		}
	}

	return b, nil
}

func (c *Compiler) prepare(ctx context.Context, a Arch, p *ir.Package) (*Context, error) {
	cc := &Context{
		a: a,
		p: p,
	}

	return cc, nil
}

func (c *Compiler) compileFunc(ctx context.Context, b []byte, cc *Context, f *ir.Func) (_ []byte, err error) {
	tlog.Printw("compile func", "name", f.Name, "in", f.In, "out", f.Out)

	err = c.prepareFunc(ctx, cc, f)
	if err != nil {
		return nil, errors.Wrap(err, "prepare")
	}

	p := cc.p

	base := f.Code[0]
	funcNum := 0
	for p.Funcs[funcNum] != f {
		funcNum++
	}

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

	for _, id := range f.Code {
		x := p.Exprs[id]

		if l, ok := x.(ir.Label); ok || next {
			bb++
			next = false

			setLabel(l, bb, ir.Expr(id))
		}

		i2b[id-base] = bb

		//	tlog.Printw("func code", "bb", bb, "id", id, "typ", tlog.FormatNext("%T"), x, "val", x)

		if _, ok := x.(ir.B); ok {
			next = true
		}
		if _, ok := x.(ir.BCond); ok {
			next = true
		}
	}

	loop := make([]int, len(f.Code))

	for _, id := range f.Code {
		x := p.Exprs[id]
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

		dset := func(ids ...ir.Expr) {
			for _, id := range ids {
				d.Set(id - base)
			}
		}

		for _, p := range f.Out {
			dset(p.Expr)
		}

		labelneed := make([]Set, len(l2i))

		for run := 0; run < 2; run++ {
			d.Reset()

			for i := ir.Expr(len(f.Code)) - 1; i >= 0; i-- {
				id := f.Code[i]
				x := p.Exprs[id]

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
					slots[id-base] = d
				}

				switch x := x.(type) {
				case ir.Imm, ir.Arg:
				case ir.Label:
					labelneed[x] = d
				case ir.Phi:
					for _, xx := range x {
						dset(xx)
					}
				case ir.B:
				case ir.BCond:
					dset(x.Expr)
				case ir.Cmp:
					dset(x.L, x.R)
				case ir.Add:
					dset(x.L, x.R)
				case ir.Sub:
					dset(x.L, x.R)
				case ir.Mul:
					dset(x.L, x.R)
				case ir.Call:
					dset(x.In...)
				default:
					panic(x)
				}

				d.Clear(id - base)
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
			d.Set(p.Expr - base)
		}

		labelhave := make([]Set, len(l2i))

		for run := 0; run < 2; run++ {
			d.Reset()

			check := func(ids ...ir.Expr) {
				if run == 0 {
					return
				}

				for _, id := range ids {
					if !d.IsSet(id - base) {
						panic(id)
					}
				}
			}

			for _, id := range f.Code {
				x := p.Exprs[id]

				d.Set(id - base)

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
						d.Clear(xx - base)
					}
				case ir.Cmp:
					check(x.L, x.R)
				case ir.Add:
					check(x.L, x.R)
				case ir.Sub:
					check(x.L, x.R)
				case ir.Mul:
					check(x.L, x.R)
				case ir.Call:
					check(x.In...)
					// TODO
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
					slots[id-base].And(d)
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

		for i := ir.Expr(len(f.Code)) - 1; i >= 0; i-- {
			id := f.Code[i]
			x := p.Exprs[id]

			switch x.(type) {
			case ir.Label:
			case ir.Phi:
				d.Set(id - base)
				continue
			default:
				continue
			}

			slots[id-base] = d

			for j := id + 1; int(j) < len(f.Code); j++ {
				id := f.Code[j]
				x := p.Exprs[id]

				if _, ok := x.(ir.Phi); !ok {
					break
				}

				slots[j] = d
			}

			d.Reset()
		}
	}

	edges := make([]Set, len(f.Code))
	clique := 0

	for _, id := range f.Code {
		if s := slots[id-base].Size(); s > clique {
			clique = s
		}

		slots[id-base].Range(func(x ir.Expr) bool {
			slots[id-base].Range(func(y ir.Expr) bool {
				if x == y {
					return true
				}

				//	if _, ok := f.Code[x].(ir.Phi); ok {
				//		return
				//	}

				edges[x].Set(y)
				edges[y].Set(x)

				return true
			})

			return true
		})
	}

	tlog.Printw("graph", "clique", clique)

	for id, d := range edges {
		if d.Size() == 0 {
			continue
		}

		tlog.Printw("edges", "id", id, "x", d)
	}

	for _, id := range f.Code {
		x := p.Exprs[id]

		if phi, ok := x.(ir.Phi); ok {
			tlog.Printw("phi", "id", id, "phi", phi)
		}
	}

	if clique > 20 {
		// TODO: spill if registers_count < clique
		panic(clique)
	}

	color := c.colorGraph(edges, slots, p, f)

	regmap := make([]int, clique)

	regs := map[int]struct{}{}
	cols := map[int]struct{}{}

	for i := 0; i < clique; i++ {
		regs[i] = struct{}{}
		cols[i] = struct{}{}
	}

	for i, p := range f.In {
		col := color[p.Expr-base]
		regmap[col] = i

		delete(regs, i)
		delete(cols, col)
	}

	for i, p := range f.Out {
		if _, ok := regs[i]; !ok {
			continue
		}

		col := color[p.Expr]
		if _, ok := cols[col]; !ok {
			continue
		}

		regmap[col] = i

		delete(regs, i)
		delete(cols, col)
	}

	for i := range regs {
		if _, ok := cols[i]; ok {
			regmap[i] = i

			delete(regs, i)
			delete(cols, i)

			continue
		}

		for col := range cols {
			regmap[col] = i

			delete(regs, i)
			delete(cols, col)

			break
		}
	}

	tlog.Printw("regmap", "col2reg", regmap)

	phifix := map[int]map[int][][2]int{} // block-to -> block-from -> list of {DST, SRC regs}

	for _, id := range f.Code {
		x := p.Exprs[id]

		phi, ok := x.(ir.Phi)
		if !ok {
			continue
		}

		if color[id-base] < 0 {
			color[id-base] = color[phi[0]-base]
		}

		colors := make([]int, len(phi))
		for j, xx := range phi {
			colors[j] = color[xx-base]
		}

		tlog.Printw("phi", "id", id, "phi", phi, "color", color[id-base], "others", colors)

		for _, xx := range phi {
			if color[id-base] != color[xx-base] {
				to := i2b[id-base]
				from := i2b[xx-base]

				if phifix[to] == nil {
					phifix[to] = map[int][][2]int{}
				}

				phifix[to][from] = append(phifix[to][from], [2]int{
					regmap[color[id-base]],
					regmap[color[xx-base]],
				})
			}
		}
	}

	for _, id := range f.Code {
		x := p.Exprs[id]

		var reg any = tlog.None
		var col any = tlog.None

		switch x.(type) {
		case ir.B, ir.BCond, ir.Label:
		default:
			c := color[id-base]
			col = c

			if c != -1 {
				reg = regmap[c]
			}
		}

		var slots2 Set

		slots[id-base].Range(func(id ir.Expr) bool {
			slots2.Set(base + id)

			return true
		})

		tlog.Printw("func code", "bb", i2b[id-base], "loop", loop[id-base], "id", id, "typ", tlog.FormatNext("%T"), x, "val", x, "reg", reg, "color", col, "slots", slots2)
	}

	{ // codegen
		reg := func(id ir.Expr) int {
			col := color[id-base]
			return regmap[col]
		}

		fixName := func(to, from int) string {
			return fmt.Sprintf(".fix.%d.%d", to, from)
		}

		fixLabel := func(br ir.Expr, l ir.Label) string {
			to := i2b[l2i[l]-base]
			from := i2b[br-base]

			if len(phifix[to][from]) == 0 {
				return ""
			}

			return fixName(to, from)
		}

		labelName := func(l ir.Label) string {
			return fmt.Sprintf("L.f%v.l%v", funcNum, l)
		}

		b = fmt.Appendf(b, `
.global _%v
.align 4
_%[1]v:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP

`, f.Name)

		for _, id := range f.Code {
			x := p.Exprs[id]

			switch x := x.(type) {
			case ir.Arg:
				b = fmt.Appendf(b, "	// reg %d  arg %d  expr %d\n", reg(id), x, id)
			case ir.Imm:
				b = fmt.Appendf(b, "	MOV	X%d, #%d	// expr %d\n", reg(id), x, id)
			case ir.Add:
				b = fmt.Appendf(b, "	ADD	X%d, X%d, X%d	// expr %d\n", reg(id), reg(x.L), reg(x.R), id)
			case ir.Sub:
				b = fmt.Appendf(b, "	SUB	X%d, X%d, X%d	// expr %d\n", reg(id), reg(x.L), reg(x.R), id)
			case ir.Mul:
				b = fmt.Appendf(b, "	MUL	X%d, X%d, X%d	// expr %d\n", reg(id), reg(x.L), reg(x.R), id)
			case ir.Cmp:
				b = fmt.Appendf(b, "	CMP	X%d, X%d	// expr %d\n", reg(x.L), reg(x.R), id)
			case ir.B:
				fix := fixLabel(id, x.Label)

				b = fmt.Appendf(b, "	B	%v%v\n", labelName(x.Label), fix)
			case ir.BCond:
				fix := fixLabel(id, x.Label)

				b = fmt.Appendf(b, "	B.%v	%v%v\n", cond2asm(x.Cond), labelName(x.Label), fix)
			case ir.Label:
				for from, list := range phifix[i2b[id-base]] {
					fix := fixName(i2b[id-base], from)

					b = fmt.Appendf(b, "%v%v:\n", labelName(x), fix)

					b = dephi(b, list)

					b = fmt.Appendf(b, "	B	%v\n", labelName(x))
				}

				b = fmt.Appendf(b, "%v:\n", labelName(x))
			case ir.Phi:
			case ir.Call:
				b = fmt.Appendf(b, "	// args ")

				for i, id := range x.In {
					if i != 0 {
						b = fmt.Appendf(b, ", ")
					}

					b = fmt.Appendf(b, "%d (X%d)", id, reg(id))
				}

				b = fmt.Appendf(b, "\n")

				b = fmt.Appendf(b, "	BL	_%v	// expr %d\n", x.Func, id)

				b = fmt.Appendf(b, "	// result %d (X%d)\n", id, reg(id))
			default:
				panic(x)
			}
		}

		for i, p := range f.Out {
			if reg(p.Expr) != i {
				b = fmt.Appendf(b, "	MOV	X%d, X%d	// res %d  expr %d\n", i, reg(p.Expr), i, p.Expr)
			} else {
				b = fmt.Appendf(b, "	// reg %d  res %d  expr %d\n", reg(p.Expr), i, p.Expr)
			}
		}

		b = fmt.Appendf(b, `
	LDP     FP, LR, [SP], #16
	RET
`)
	}

	return b, nil
}

func (c *Compiler) prepareFunc(ctx context.Context, cc *Context, f *ir.Func) (err error) {
	expr := func(i int) (id ir.Expr, x any) {
		id = f.Code[i]
		x = cc.p.Exprs[id]
		return
	}

	insert := func(i int, x any) {
		ee := cc.p.Exprs

		id := ir.Expr(len(ee))

		ee = append(ee, x)
		copy(ee[i+1:], ee[i:])
		ee[i] = id

		cc.p.Exprs = ee
	}

	for i := 0; i < len(f.Code); i++ {
		_, x := expr(i)

		switch x := x.(type) {
		case ir.Label:
			if i == 0 {
				break
			}

			_, px := expr(i - 1)

			if _, ok := px.(ir.B); !ok {
				insert(i, ir.B{Label: x})
			}
		case ir.Call:
		default:
		}
	}

	return nil
}

func (c *Compiler) colorGraph(edges []Set, slots []Set, p *ir.Package, f *ir.Func) []int {
	base := f.Code[0]
	color := make([]int, len(f.Code))

	for i := range color {
		color[i] = -1

	}

	phi := make([]Set, len(f.Code))

	for _, id := range f.Code {
		x := p.Exprs[id]

		if p, ok := x.(ir.Phi); ok {
			phi[id-base].Set(p...)

			for _, p := range p {
				phi[p-base].Set(id - base)
			}
		}
	}

	max := 0

	for _, d := range slots {
		if s := d.Size(); s > max {
			max = s
		}
	}

	q := make([]ir.Expr, 0, len(f.Code))

	for _, id := range f.Code {
		x := p.Exprs[id]

		switch x.(type) {
		case ir.Label, ir.B, ir.BCond:
			continue
		default:
		}

		q = append(q, id)
	}

	l := make([]int, len(q))
	l[0] = len(q)

	findV := func(i int) ir.Expr {
		for j := i; j < i+l[i]; j++ {
			id := q[j]
			if color[id] == -1 {
				continue
			}

			q[i], q[j] = q[j], q[i]

			break
		}

		return q[i]
	}

	sortRange := func(v ir.Expr, s, e int) {
		//	tlog.Printw("sort range", "v", v, "s", s, "e", e, "of", len(q), "edges", edges[v])
		d := edges[v-base]
		i := s
		j := e

		for i != j {
			//	tlog.Printw("loop", "i", i, "j", j, "L", d.IsSet(q[i]), "R", d.IsSet(q[j-1]), "q", q[i:j])
			if d.IsSet(q[i] - base) {
				i++
			} else if !d.IsSet(q[j-1] - base) {
				j--
			} else {
				q[i], q[j-1] = q[j-1], q[i]
				i++
				j--
			}
		}

		if i-s != 0 {
			l[s] = i - s
		}

		if e-i != 0 {
			l[i] = e - i
		}
	}

	assignable := func(id ir.Expr, col int) bool {
		ok := true

		edges[id-base].Range(func(xx ir.Expr) bool {
			if color[xx] == col {
				ok = false
			}

			return ok
		})

		return ok
	}

	assign := func(id ir.Expr, col int) {
		if col < 0 {
			panic(id)
		}

		if color[id-base] >= 0 {
			panic(id)
		}

		color[id-base] = col
	}

	var walk func(ir.Expr, *Set) Set
	walk = func(id ir.Expr, visited *Set) (r Set) {
		if visited.IsSet(id - base) {
			return
		}

		visited.Set(id - base)

		c := color[id-base]
		tlog.V("walk").Printw("walk", "id", id, "c", c, "neighbours", phi[id-base])
		if c != -1 {
			r.Set(ir.Expr(c))
			return
		}

		phi[id-base].Range(func(x ir.Expr) bool {
			sub := walk(x, visited)
			r.Or(sub)

			return true
		})

		return
	}

	i := 0
	for i < len(q) {
		if tlog.If("state") {
			var args []any
			for j := i; j < len(q); j += l[j] {
				args = append(args, fmt.Sprintf("%d", j), q[j:j+l[j]])
			}

			tlog.Printw("state", args...)
		}

		id := findV(i)

		col := 0
		var cols Set

		for col < max {
			if assignable(id, col) {
				cols.Set(ir.Expr(col))
			}

			col++
		}

		col = int(cols.First())

		var walked Set
		if p := phi[id-base]; p.Size() != 0 {
			walked = walk(id, new(Set))
			if f := walked.AndCp(cols).First(); f != -1 {
				col = int(f)
			}
		}

		tlog.Printw("color", "id", id, "color", col, "colors", cols, "phi", phi[id-base], "walk", walked)

		assign(id, col)

		if l[i] != 1 {
			l[i+1] = l[i] - 1
		}

		l[i] = 1
		i++

		o := 7
		for j := i; j < len(q) && o > 0; o-- {
			tlog.V("sort_range").Printw("sort range", "s", j, "e", j+l[j], "edges", edges[id-base])

			e := j + l[j]

			if j-e != 1 {
				sortRange(id, j, e)
			}

			j = e
		}
	}

	return color
}

func (d *Set) Set(ids ...ir.Expr) *Set {
	for _, id := range ids {
		*d |= 1 << id
	}

	return d
}

func (d *Set) Clear(ids ...ir.Expr) *Set {
	for _, id := range ids {
		*d &^= 1 << id
	}

	return d
}

func (d *Set) Or(x Set) *Set {
	*d |= x

	return d
}

func (d *Set) And(x Set) *Set {
	*d &= x

	return d
}

func (d Set) OrCp(x Set) Set {
	return d & x
}

func (d Set) AndCp(x Set) Set {
	return d & x
}

func (d Set) AndNotCp(x Set) Set {
	return d &^ x
}

func (d Set) XorCp(x Set) Set {
	return d ^ x
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

func (d Set) Range(f func(id ir.Expr) bool) {
	for i := 0; i < 64; i++ {
		if d&(1<<i) == 0 {
			continue
		}

		if !f(ir.Expr(i)) {
			break
		}
	}
}

func (d Set) First() ir.Expr {
	for i := 0; i < 64; i++ {
		if d&(1<<i) == 0 {
			continue
		}

		return ir.Expr(i)
	}

	return -1
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

func dephi(b []byte, l [][2]int) []byte {
	b = fmt.Appendf(b, "	// swap %v\n", l)

next:
	for i := 0; i < len(l); i++ {
		for j := i + 1; j < len(l); j++ {
			if l[i][0] != l[j][1] {
				continue
			}

			b = swap(b, l[i][0], l[j][0])
			l[j][1], l[i][1] = l[i][1], l[j][1]

			continue next
		}

		p := l[i]

		if p[0] == p[1] {
			continue
		}

		b = fmt.Appendf(b, "	MOV	X%d, X%d\n", p[0], p[1])
	}

	return b
}

func swap(b []byte, x, y int) []byte {
	b = fmt.Appendf(b, "	// swap X%d, X%d\n", x, y)
	b = fmt.Appendf(b, "	EOR	X%d, X%d, X%d\n", x, x, y)
	b = fmt.Appendf(b, "	EOR	X%d, X%d, X%d\n", y, x, y)
	b = fmt.Appendf(b, "	EOR	X%d, X%d, X%d\n", x, x, y)
	return b
}

func cond2asm(cond ir.Cond) string {
	switch cond {
	case "<":
		return "LT"
	case ">":
		return "GT"
	case "<=":
		return "LE"
	case ">=":
		return "GE"
	case "==":
		return "EQ"
	case "!=":
		return "NE"
	default:
		panic(cond)
	}
}
