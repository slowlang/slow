//go:build ignore

package back

import (
	"context"
	"fmt"
	"path"
	"sort"

	"tlog.app/go/errors"
	"tlog.app/go/loc"
	"tlog.app/go/tlog"
	"github.com/slowlang/slow/src/compiler/bitmap"
	"github.com/slowlang/slow/src/compiler/ir"
	"github.com/slowlang/slow/src/compiler/tp"
)

type (
	Arch interface{}

	Compiler struct {
	}

	pkgContext struct {
		Arch
		*ir.Package

		f *ir.Func

		phi map[ir.Expr]ir.Phi

		i2b  map[ir.Expr]int
		l2i  map[ir.Label]ir.Expr
		loop map[ir.Expr]int

		loopmax int

		slots map[ir.Expr]bitmap.Big
		edges map[ir.Expr]bitmap.Big

		regs   map[ir.Expr]Reg
		phifix map[int]map[int][][2]Reg // to -> from -> []{to_reg, from_reg}

		calleeSaved Reg

		lab ir.Label
	}

	Reg int
)

func New() *Compiler {
	return nil
}

func (c *Compiler) CompilePackage(ctx context.Context, a Arch, b []byte, pkg *ir.Package) (_ []byte, err error) {
	p := &pkgContext{
		Arch:    a,
		Package: pkg,

		calleeSaved: 19,
	}

	for _, x := range pkg.Exprs {
		if l, ok := x.(ir.Label); ok {
			if l+1 > p.lab {
				p.lab = l + 1
			}
		}
	}

	if tlog.If("dump_pkg") {
		tlog.Printw("package", "name", path.Base(p.Path), "path", p.Path)

		for id, x := range p.Exprs {
			tlog.Printw("expr", "id", id, "tp", p.EType[id], "typ", tlog.NextIsType, x, "val", x)
		}
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

	for _, fid := range p.Funcs {
		f := p.Exprs[fid].(*ir.Func)

		b = append(b, '\n')

		b, err = c.compileFunc(ctx, b, p, f)
		if err != nil {
			return nil, errors.Wrap(err, "func %v", f.Name)
		}
	}

	return b, nil
}

func (c *Compiler) compileFunc(ctx context.Context, b []byte, p *pkgContext, f *ir.Func) (_ []byte, err error) {
	tlog.Printw("compile func", "name", f.Name, "in", f.In, "out", f.Out, "state_in", f.StateIn, "state_out", f.StateOut)

	if tlog.If("dump_func_src") {
		for i, id := range f.Code {
			x := p.Exprs[id]

			tlog.Printw("code", "i", i, "id", id, "typ", tlog.NextIsType, x, "val", x)
		}
	}

	p.f = f

	err = c.fixFunc(ctx, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "fix func")
	}

	err = c.analyzeFunc(ctx, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "analyze func")
	}

	err = c.buildGraph(ctx, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "analyze func")
	}

	err = c.colorGraph(ctx, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "analyze func")
	}

	err = c.fixPhi(ctx, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "analyze func")
	}

	b, err = c.codegenFunc(ctx, b, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "analyze func")
	}

	return b, nil
}

func (c *Compiler) fixFunc(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	inserted := 0

	expr := func(i int) (id ir.Expr, x any) {
		if i < 0 {
			return -1, nil
		}

		id = f.Code[i]
		x = p.Exprs[id]
		return
	}

	insert := func(i int, xxx ...any) int {
		if tlog.If("dump") {
			tlog.Printw("function before fix", "name", f.Name)

			for i, id := range f.Code {
				x := p.Exprs[id]

				tlog.Printw("code", "i", i, "id", id, "typ", tlog.NextIsType, x, "val", x)
			}
		}
		tlog.Printw("insert fix", "at", i, "insert", xxx, "from", loc.Caller(1))

		f.Code = append(f.Code, make([]ir.Expr, len(xxx))...)
		copy(f.Code[i+len(xxx):], f.Code[i:])

		for j, x := range xxx {
			id := p.alloc(x)
			f.Code[i+j] = id
		}

		inserted += len(xxx)

		return len(xxx)
	}

	insertIDs := func(i int, xxx ...ir.Expr) int {
		if tlog.If("dump") {
			tlog.Printw("function before fix", "name", f.Name)

			for i, id := range f.Code {
				x := p.Exprs[id]

				tlog.Printw("code", "i", i, "id", id, "typ", tlog.NextIsType, x, "val", x)
			}
		}
		tlog.Printw("insert fix", "at", i, "insert", xxx, "from", loc.Caller(1))

		f.Code = append(f.Code, make([]ir.Expr, len(xxx))...)
		copy(f.Code[i+len(xxx):], f.Code[i:])

		inserted += copy(f.Code[i:], xxx)

		return len(xxx)
	}

	_ = insertIDs

	for i := 0; i < len(f.Code); i++ {
		_, x := expr(i)

		switch x := x.(type) {
		case ir.Label:
			_, px := expr(i - 1)

			if _, ok := px.(ir.B); !ok {
				insert(i, ir.B{Label: x})
			}
		case ir.Call:
			break
		default:
		}
	}

	if tlog.If("fixed_func,dump") {
		tlog.Printw("function fixed", "name", f.Name, "inserted", inserted)

		for i, id := range f.Code {
			x := p.Exprs[id]

			tlog.Printw("code", "i", i, "id", id, "typ", tlog.NextIsType, x, "val", x)
		}
	}

	return nil
}

func (c *Compiler) analyzeFunc(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	p.i2b = map[ir.Expr]int{}
	p.l2i = map[ir.Label]ir.Expr{}
	l2p := map[ir.Label]int{}

	bb := 0
	next := false
	//	l2b := map[ir.Label]int{}

	for i, id := range f.Code {
		x := p.Exprs[id]

		if l, ok := x.(ir.Label); ok || next {
			bb++
			next = false

			if ok {
				p.l2i[l] = id
				l2p[l] = i
				//	l2b[l] = bb
			}
		}

		p.i2b[id] = bb

		//	tlog.Printw("func code", "bb", bb, "id", id, "typ", tlog.FormatNext("%T"), x, "val", x)

		if _, ok := x.(ir.B); ok {
			next = true
		}
		if _, ok := x.(ir.BCond); ok {
			next = true
		}
	}

	if tlog.If("cfg") {
		tlog.Printw("function cfg", "name", f.Name)

		for i, id := range f.Code {
			x := p.Exprs[id]

			switch x.(type) {
			case ir.B, ir.BCond, ir.Label:
			default:
				continue
			}

			tlog.Printw("code", "i", i, "id", id, "typ", tlog.NextIsType, x, "val", x)
		}
	}

	/*
		p.loop = map[ir.Expr]int{}

		for i, id := range f.Code {
			x := p.Exprs[id]
			l := ir.Label(-1)

			switch x := x.(type) {
			case ir.B:
				l = x.Label
			case ir.BCond:
				l = x.Label
			}

			if l < 0 {
				continue
			}

			for j := l2p[l]; j <= i; j++ {
				p.loop[f.Code[j]]++
			}
		}

		for _, m := range p.loop {
			if m > p.loopmax {
				p.loopmax = m
			}
		}
	*/

	return nil
}

func (c *Compiler) buildGraph(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	p.slots = make(map[ir.Expr]bitmap.Big, len(f.Code))

	var d bitmap.Big

	dset := func(ids ...ir.Expr) {
		for _, id := range ids {
			d.Set(int(id))
		}
	}

	slotsOr := func(id ir.Expr, d bitmap.Big) {
		q := p.slots[id]
		q.Or(d)
		p.slots[id] = q
	}

	// forward run

	labelhave := map[ir.Label]bitmap.Big{}

	labelOr := func(l ir.Label, d bitmap.Big) {
		q := labelhave[l]
		q.Or(d)
		labelhave[l] = q
	}

	for run := 0; run < 2; run++ {
		d.Reset()

		//	for _, p := range f.In {
		//		d.Set(int(p.Expr))
		//	}

		for _, id := range f.Code {
			x := p.Exprs[id]

			d.Set(int(id))

			switch x := x.(type) {
			case ir.Imm, ir.Args, ir.Out, ir.State:
			case ir.Cmp, ir.Add, ir.Sub, ir.Mul:
			case ir.Ptr, ir.Load, ir.Offset:
			case ir.Store:
			case ir.Alloc:
			case ir.Label:
				d.Or(labelhave[x])
			case ir.B:
				labelOr(x.Label, d)
			case ir.BCond:
				labelOr(x.Label, d)
			case ir.Phi:
				for _, xx := range x {
					d.Clear(int(xx))
				}

				d.Set(int(id))
			case ir.Call:
				// TODO
			case tp.Array, tp.Int:
			default:
				panic(x)
			}

			if tlog.If("slot_pass") {
				//	switch x.(type) {
				//	case ir.Label, ir.B, ir.BCond:
				tlog.Printw("forward pass", "run", run, "id", id, "slots", d)
				//	}
			}

			if run != 0 {
				slotsOr(id, d)
			}

			switch x.(type) {
			case ir.B:
				d.Reset()
			}
		}

		tlog.V("slot_pass").Printw("forward, label have", "labelhave", labelhave)
	}

	// backward run

	labelneed := map[ir.Label]bitmap.Big{}

	for run := 0; run < 2; run++ {
		d.Reset()

		dset(f.Out...)

		for i := ir.Expr(len(f.Code)) - 1; i >= 0; i-- {
			id := f.Code[i]
			x := p.Exprs[id]

			switch x := x.(type) {
			case ir.B:
				d = labelneed[x.Label].Copy()
			case ir.BCond:
				d.Or(labelneed[x.Label])
			}

			if tlog.If("slot_pass") {
				//	switch x.(type) {
				//	case ir.Label, ir.B, ir.BCond:
				tlog.Printw("backward pass", "run", run, "id", id, "slots", d)
				//	}
			}

			if run != 0 {
				switch x.(type) {
				case ir.Label, ir.Phi:
				default:
					p.slots[id].And(d)
				}
			}

			d.Clear(int(id))

			switch x := x.(type) {
			case ir.Imm, ir.Args, ir.Out, ir.State:
			case ir.Label:
				labelneed[x] = d.Copy()
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
				dset(x.Args...)
			case ir.Ptr:
				dset(x.X)
			case ir.Load:
				dset(x.Ptr)
			case ir.Offset:
				dset(x.Base, x.Offset, x.Size)
			case ir.Store:
				dset(x.Ptr, x.Val)
			case ir.Alloc:
				dset(ir.Expr(x.Type))
			case tp.Int:
			case tp.Array:
				dset(ir.Expr(x.Elem), x.Len)
			default:
				panic(x)
			}
		}

		tlog.V("slot_pass").Printw("backward, label need", "labelheed", labelneed)
	}

	d.Reset()

	// phi run

	for i := len(f.Code) - 1; i >= 0; i-- {
		id := f.Code[i]
		x := p.Exprs[id]

		switch x.(type) {
		case ir.Label:
		case ir.Phi:
			d.Set(int(id))
			continue
		default:
			continue
		}

		p.slots[id] = d.Copy()

		for j := i + 1; j < len(f.Code); j++ {
			id := f.Code[j]
			x := p.Exprs[id]

			if _, ok := x.(ir.Phi); !ok {
				break
			}

			p.slots[id] = d.Copy()
		}

		d.Reset()
	}

	p.edges = make(map[ir.Expr]bitmap.Big, len(f.Code))
	clique := 0

	xedges := func(x, y int) {
		q := p.edges[ir.Expr(x)]
		q.Set(y)
		p.edges[ir.Expr(x)] = q

		q = p.edges[ir.Expr(y)]
		q.Set(x)
		p.edges[ir.Expr(y)] = q
	}

	for _, id := range f.Code {
		if s := p.slots[id].Size(); s > clique {
			clique = s
		}

		p.slots[id].Range(func(x int) bool {
			p.slots[id].Range(func(y int) bool {
				if x == y {
					return true
				}

				//	if _, ok := f.Code[x].(ir.Phi); ok {
				//		return
				//	}

				xedges(x, y)

				return true
			})

			return true
		})
	}

	tlog.Printw("graph", "clique", clique)

	for id, d := range p.edges {
		if d.Size() == 0 {
			continue
		}

		tlog.Printw("edges", "id", id, "x", d)
	}

	p.phi = map[ir.Expr]ir.Phi{}

	for _, id := range f.Code {
		x := p.Exprs[id]

		if phi, ok := x.(ir.Phi); ok {
			tlog.Printw("phi", "id", id, "phi", phi)
			p.phi[id] = phi
		}
	}

	if tlog.If("dump") {
		tlog.Printw("slots calculated")

		for _, id := range f.Code {
			x := p.Exprs[id]

			tlog.Printw("code", "id", id, "typ", tlog.NextIsType, x, "val", x, "slots", p.slots[id])
		}
	}

	return nil
}

func (c *Compiler) colorGraph(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	p.regs = map[ir.Expr]Reg{}

	edges := make(map[ir.Expr]bitmap.Big)
	pregs := map[ir.Expr]Reg{}

	var all bitmap.Big

	all.FillSet(0, 28)
	all.Clear(18) // apple says don't use

	q := make([]ir.Expr, 0, len(f.Code))

	for _, id := range f.Code {
		x := p.Exprs[id]

		switch x.(type) {
		case ir.Label, ir.B, ir.BCond, ir.State, ir.Store:
			continue
		default:
		}

		q = append(q, id)
	}

	l := make([]int, len(q))
	l[0] = len(q)

	want := map[ir.Expr]Reg{}
	phi := map[ir.Expr]bitmap.Big{}

	xphi := func(x, y ir.Expr) {
		q := phi[x]
		q.Set(int(y))
		phi[x] = q

		q = phi[y]
		q.Set(int(x))
		phi[y] = q
	}

	for id, p := range p.phi {
		for _, x := range p {
			xphi(id, x)
		}
	}

	for id, d := range phi {
		tlog.Printw("phi connected", "id", id, "d", d)
	}

	var walk func(ir.Expr, *bitmap.Big) bitmap.Big
	walk = func(id ir.Expr, visited *bitmap.Big) (res bitmap.Big) {
		if visited == nil {
			visited = bitmap.New()
		}

		if visited.IsSet(int(id)) {
			return
		}

		visited.Set(int(id))

		if r, ok := p.regs[id]; ok {
			res.Set(int(r))
			return
		}

		phi[id].Range(func(x int) bool {
			sub := walk(ir.Expr(x), visited)
			res.Or(sub)

			return true
		})

		return
	}

	findV := func(i int) ir.Expr {
		//	sort.SliceStable(q[i:i+l[i]], func(l, r int) bool {
		//		return phi[q[i+l]].Size() > phi[q[i+r]].Size()
		//	})

		return q[i]
	}

	splitRange := func(s, e int, left func(ir.Expr) bool) {
		//	tlog.Printw("sort range", "v", v, "s", s, "e", e, "of", len(q), "edges", edges[v])
		if e-s <= 1 {
			return
		}

		b := make([]ir.Expr, e-s)

		i := 0
		for j := s; j < e; j++ {
			if left(q[j]) {
				b[i] = q[j]
				i++
			}
		}

		m := s + i

		for j := s; j < e; j++ {
			if !left(q[j]) {
				b[i] = q[j]
				i++
			}
		}

		copy(q[s:], b)

		if m-s != 0 {
			l[s] = m - s
		}

		if e-m != 0 {
			l[m] = e - m
		}
	}

	splitAdjacent := func(v ir.Expr, s, e int) {
		d := p.edges[v]
		d = d.OrCopy(edges[v])

		splitRange(s, e, func(id ir.Expr) bool {
			return d.IsSet(int(id))
		})
	}

	color := func(id ir.Expr) {
		if _, ok := p.regs[id]; ok {
			panic(id)
		}

		var used bitmap.Big

		ee := p.edges[id]
		ee = ee.OrCopy(edges[id])

		ee.Range(func(x int) bool {
			if r, ok := p.regs[ir.Expr(x)]; ok {
				used.Set(int(r))
			}

			if r, ok := pregs[ir.Expr(x)]; ok {
				used.Set(int(r))
			}

			return true
		})

		available := all.AndNotCopy(used)

		var reg Reg = -1
		var wanted any = tlog.None

		if r, ok := want[id]; ok {
			wanted = r

			if available.IsSet(int(r)) {
				reg = r
			}
		}

		var walked bitmap.Big

		if reg == -1 {
			walked = walk(id, nil)
			want := available.AndCopy(walked)

			reg = Reg(want.First())
		}

		if reg == -1 {
			reg = Reg(available.First())
		}

		tlog.Printw("choose color", "id", id, "reg", reg, "used", used, "wanted", wanted, "walked", walked)

		if reg == -1 {
			panic(id)
		}

		p.regs[id] = reg
	}

	// reserve registers

	base := len(p.Exprs)

	xedges := func(x, y int) {
		q := edges[ir.Expr(x)]
		q.Set(y)
		edges[ir.Expr(x)] = q

		q = edges[ir.Expr(y)]
		q.Set(x)
		edges[ir.Expr(y)] = q
	}

	for _, id := range f.Code {
		x := p.Exprs[id]

		switch x.(type) {
		case ir.Call:
			p.slots[id].Range(func(x int) bool {
				if x == int(id) {
					return true
				}

				for i := 0; i < int(p.calleeSaved); i++ {
					xedges(x, base+i)
				}

				return true
			})
		}
	}

	for i := 0; i < int(p.calleeSaved); i++ {
		pregs[ir.Expr(base+i)] = Reg(i)
	}

	tlog.Printw("reserved edges", "base", base, "reserve", edges, "pregs_start", base, "pregs_end", base+int(p.calleeSaved))

	// prealloc

	{
		//	for i, p := range f.In {
		//		want[p.Expr] = Reg(i)
		//	}

		for i, p := range f.Out {
			want[p] = Reg(i)
		}

		for _, id := range f.Code {
			x := p.Exprs[id]

			switch x := x.(type) {
			case ir.Args:
				want[id] = Reg(0)
			case ir.Out:
				want[id] = Reg(x)
			case ir.Call:
				for i, a := range x.Args {
					want[a] = Reg(i)
				}

				want[id] = 0
			}
		}

		// TODO: can we split here?
		//	splitRange(0, len(q), func(id ir.Expr) bool {
		//		_, ok := want[id]
		//
		//		return ok
		//	})

		sort.SliceStable(q, func(l, r int) bool {
			_, okl := want[q[l]]
			_, okr := want[q[r]]

			if okl != okr {
				return okl
			}

			return phi[q[l]].Size() > phi[q[r]].Size()
		})
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

		if !p.slots[id].IsSet(int(id)) {
			tlog.Printw("skip unused expr", "id", id)

			if l[i] != 1 {
				l[i+1] = l[i] - 1
			}

			l[i] = 1
			i++

			continue
		}

		color(id)

		if l[i] != 1 {
			l[i+1] = l[i] - 1
		}

		l[i] = 1
		i++

		for j := i; j < len(q); {
			e := j + l[j]

			if j-e != 1 {
				splitAdjacent(id, j, e)
			}

			j = e
		}
	}

	tlog.Printw("registers allocated", "name", f.Name, "in", f.In, "out", f.Out, "state_in", f.StateIn, "state_out", f.StateOut)

	for _, id := range f.Code {
		x := p.Exprs[id]
		var reg any = tlog.None

		if r, ok := p.regs[id]; ok {
			reg = r
		}

		tlog.Printw("code", "bb", p.i2b[id], "id", id, "tp", p.EType[id], "typ", tlog.NextIsType, x, "val", x, "reg", reg, "slots", p.slots[id])
	}

	return nil
}

func (c *Compiler) fixPhi(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	p.phifix = map[int]map[int][][2]Reg{}

	findFromB := func(dstid, xx ir.Expr) int {
		lab := ir.Label(-1)

		for _, id := range f.Code {
			x := p.Exprs[id]
			if l, ok := x.(ir.Label); ok {
				lab = l
			}

			if id == dstid {
				break
			}
		}

		for i, id := range f.Code {
			if id != xx {
				continue
			}

			for _, id := range f.Code[i:] {
				x := p.Exprs[id]

				switch x := x.(type) {
				case ir.BCond:
					if x.Label == lab {
						return p.i2b[id]
					}
				case ir.B:
					if x.Label == lab {
						return p.i2b[id]
					}
				}
			}
		}

		panic(xx)
	}

	for id, x := range p.phi {
		reg := p.regs[id]

		l := make([]Reg, len(x))

		for i, xx := range x {
			l[i] = p.regs[xx]

			if reg == l[i] {
				continue
			}

			toB := p.i2b[id]
			fromB := findFromB(id, xx)

			if p.phifix[toB] == nil {
				p.phifix[toB] = map[int][][2]Reg{}
			}

			perm := p.phifix[toB][fromB]
			perm = append(perm, [2]Reg{reg, l[i]})
			p.phifix[toB][fromB] = perm
		}

		tlog.Printw("phi", "id", id, "val", x, "reg", p.regs[id], "others", l)
	}

	for toB, x := range p.phifix {
		for fromB, l := range x {
			tlog.Printw("permutation", "block_dst", toB, "block_src", fromB, "perm", l)
		}
	}

	return nil
}

func (c *Compiler) codegenFunc(ctx context.Context, b []byte, p *pkgContext, f *ir.Func) (_ []byte, err error) {
	fixName := func(to, from int) string {
		return fmt.Sprintf(".fix.%d.%d", to, from)
	}

	fixLabel := func(br ir.Expr, l ir.Label) string {
		to := p.i2b[p.l2i[l]]
		from := p.i2b[br]

		if len(p.phifix[to][from]) == 0 {
			return ""
		}

		return fixName(to, from)
	}

	labelName := func(l ir.Label) string {
		return fmt.Sprintf("L.%v", l)
	}

	reg := func(id ir.Expr) Reg {
		return p.regs[id]
	}

	maxreg := Reg(0)

	for _, reg := range p.regs {
		if reg > maxreg {
			maxreg = reg
		}
	}

	if maxreg&1 == 1 {
		maxreg++
	}

	b = fmt.Appendf(b, `
.global _%v
.align 4
_%[1]v:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP
`, f.Name)

	stReg := p.calleeSaved

	for ; stReg < maxreg; stReg += 2 {
		b = fmt.Appendf(b, "	STP     X%d, X%d, [SP, #-16]!\n", stReg, stReg+1)
	}

	b = append(b, '\n')

	allocs := 0

	for _, id := range f.Code {
		x := p.Exprs[id]

		if !p.slots[id].IsSet(int(id)) {
			switch x.(type) {
			case ir.Imm:
				tlog.Printw("skip unused expr", "id", id)
				continue
			}
		}

		switch x := x.(type) {
		case ir.Args:
			b = fmt.Appendf(b, "	// reg %d  args %d  expr %d\n", reg(id), x, id)
		case ir.Out, ir.State:
		case ir.Imm:
			b = fmt.Appendf(b, "	MOV	X%d, #%d		// expr %d\n", reg(id), x, id)
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
			for from, list := range p.phifix[p.i2b[id]] {
				fix := fixName(p.i2b[id], from)

				b = fmt.Appendf(b, "%v%v:\n", labelName(x), fix)

				b = permutate(b, list)

				b = fmt.Appendf(b, "	B	%v\n", labelName(x))
			}

			b = fmt.Appendf(b, "%v:\n", labelName(x))
		case ir.Phi:
		case ir.Call:
			for i, id := range x.Args {
				if reg(id) == Reg(i) {
					continue
				}

				b = fmt.Appendf(b, "	MOV	X%d, X%d	// func arg %d\n", i, reg(id), i)
			}

			f := p.Exprs[x.Func].(*ir.Func)

			b = fmt.Appendf(b, "	BL	_%v	// func call  ", f.Name)

			for i, id := range x.Args {
				if i != 0 {
					b = fmt.Appendf(b, ", ")
				}

				b = fmt.Appendf(b, "%d (X%d)", id, reg(id))
			}

			b = fmt.Appendf(b, " -> %d (X%d)\n", id, reg(id))

			if r := reg(id); r != 0 {
				b = fmt.Appendf(b, "	MOV	X%d, X%d	// func res %d fix\n", reg(id), 0, 0)
			}
		case ir.Offset:
			sz := p.Exprs[x.Size].(ir.Imm)

			if sz&(sz-1) == 0 {
				lsl := pow(int(sz))
				b = fmt.Appendf(b, "	ADD	X%d, X%d, X%d, LSL %d	// expr %d\n", reg(id), reg(x.Base), reg(x.Offset), lsl, id)
			} else {
				panic(sz)
			}
		case ir.Store:
			b = fmt.Appendf(b, "	STR	X%d, [X%d]	// expr %d\n", reg(x.Val), reg(x.Ptr), id)
		case ir.Alloc:
			b = fmt.Appendf(b, "	SUB	SP, SP, #32	// alloc\n")
			b = fmt.Appendf(b, "	MOV	X%d, SP		// expr %d\n", reg(id), id)

			allocs += 32
		case ir.Load:
			b = fmt.Appendf(b, "	LDR	X%d, [X%d]	// expr %d\n", reg(id), reg(x.Ptr), id)

			//	case ir.Data:
			//	case ir.Ptr:
			//		b = fmt.Appendf(b, "	ADR	X%d, .data.%d	// expr %d\n", reg(id), x.X, id)
		case tp.Array:
		default:
			panic(x)
		}
	}

	for i, id := range f.Out {
		b = fmt.Appendf(b, "	// reg %d  res %d  expr %d\n", reg(id), i, id)
	}

	b = append(b, '\n')

	if allocs != 0 {
		b = fmt.Appendf(b, "	MOV	SP, FP		// free\n")
	}

	for stReg -= 2; stReg >= p.calleeSaved; stReg -= 2 {
		b = fmt.Appendf(b, "	LDP     X%d, X%d, [SP], #16\n", stReg, stReg+1)
	}

	b = fmt.Appendf(b, `	LDP     FP, LR, [SP], #16
	RET
`)

	b = append(b, '\n')

	for _, id := range f.Code {
		x := p.Exprs[id]

		switch x := x.(type) {
		//	case ir.Data:
		//		b = fmt.Appendf(b, ".data.%d:\n	.asciz %q\n", id, x)
		default:
			_ = x
		}
	}

	return b, nil
}

func (p *pkgContext) alloc(x any) ir.Expr {
	id := ir.Expr(len(p.Package.Exprs))
	p.Package.Exprs = append(p.Package.Exprs, x)

	return id
}

func (p *pkgContext) label() ir.Label {
	l := p.lab
	p.lab++

	return l
}

func permutate(b []byte, l [][2]Reg) []byte {
	b = fmt.Appendf(b, "	// permutate %v\n", l)

	dst := map[Reg]Reg{} // src -> dst

	for _, p := range l {
		dst[p[1]] = p[0]
	}

	var move func(d, s, loop Reg) bool
	move = func(d, s, loop Reg) bool {
		if d == loop {
			return false
		}

		if loop == -1 {
			loop = d
		}

		defer delete(dst, s)

		d2, ok := dst[d]
		if ok {
			ok = !move(d2, d, loop)
		}

		if !ok {
			b = fmt.Appendf(b, "	MOV	X%d, X%d\n", d, s)

			return true
		}

		if d == loop {
			return false
		}

		b = swap(b, d, s)

		return false
	}

	for _, p := range l {
		if _, ok := dst[p[1]]; !ok {
			continue
		}

		move(p[0], p[1], -1)
	}

	return b

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

func swap(b []byte, x, y Reg) []byte {
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

func pow(x int) (p int) {
	if x < 1 {
		panic(x)
	}

	for x > 1 {
		p++
		x >>= 1
	}

	return p
}
