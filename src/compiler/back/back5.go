package back

import (
	"context"
	"fmt"
	"sort"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/nikandfor/tlog/tlwire"
	"github.com/slowlang/slow/src/compiler/df"
	"github.com/slowlang/slow/src/compiler/ir"
	"github.com/slowlang/slow/src/compiler/set"
)

type (
	Arch interface{}

	Compiler struct {
	}

	pkgContext struct {
		Arch
		*ir.Package

		*funContext

		calleeSaved Reg
	}

	funContext struct {
		//	e2i map[ir.Expr]int
		//	e2b map[ir.Expr]int
		//	l2e map[ir.Label]ir.Expr
		//	l2i []int // ir.Label -> index
		//	lin map[ir.Label][]ir.Expr

		loop  []int // index -> depth
		slots map[ir.Expr]*set.Bitmap
		edges map[ir.Expr]*set.Bitmap
	}

	Reg int

	BChoice struct {
		B     int
		Taken int
	}

	Job struct {
		i, p    int
		need    BitsInt
		choices []BChoice
	}

	Jobs []Job

	walked int8
)

const (
	walkNone = iota
	walkCycle
	walkMerge

	walkEnd = -1
)

const (
	bTaken = 1 << iota
	bSkipped
)

func New() *Compiler {
	return nil
}

func (c *Compiler) CompilePackage(ctx context.Context, a Arch, b []byte, pkg *ir.Package) (_ []byte, err error) {
	tr, ctx := tlog.SpawnFromContextAndWrap(ctx, "back: compile package", "name", pkg.Path)
	defer tr.Finish("err", &err)

	p := &pkgContext{
		Arch:    a,
		Package: pkg,

		calleeSaved: 19,
	}

	if tr.If("dump_pkg") {
		for id, x := range p.Exprs {
			tr.Printw("expr", "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}

		for _, id := range p.Funcs {
			f := p.Exprs[id].(*ir.Func)

			tr.Printw("func", "id", id, "tp", p.EType[id], "name", f.Name, "type", p.Exprs[p.EType[id]])
		}
	}

	b = fmt.Appendf(b, `// package %s

.global _start
.align 4
_start:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP

	BL	main

	LDP     FP, LR, [SP], #16
	RET
`, pkg.Path)

	b = nil

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
	tr, ctx := tlog.SpawnFromContextAndWrap(ctx, "func", "id", func() ir.Expr {
		for _, id := range p.Funcs {
			if p.Exprs[id] == f {
				return id
			}
		}

		return -1
	}(), "name", f.Name, "in", f.In, "out", f.Out,
	/*"state", f.State, "effect", f.Effect*/)
	defer tr.Finish("err", &err)

	if tr.If("hide_func_" + f.Name) {
		tr.Printw("hide func logs")
		tr.Logger = nil
		ctx = tlog.ContextWithSpan(ctx, tr)
	}

	p.funContext = &funContext{}

	if tr.If("dump_func_before") {
		tr.Printw("func before")

		for i, id := range f.Code {
			x := p.Exprs[id]

			tr.Printw("code", "i", i, "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}
	}

	if tr.If("walkFunc") {
		err = c.walkFunc2(ctx, p, f)
		if err != nil {
			return nil, errors.Wrap(err, "walk func")
		}

		if tr.If("dump_func_walkFunc") {
			tr.Printw("func walked", "name", f.Name, "in", f.In, "out", f.Out)

			for i, id := range f.Code {
				x := p.Exprs[id]

				tr.Printw("code", "i", i, "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
			}
		}
	}

	if tr.If("findLoops") {
		err = c.findLoops(ctx, p, f)
		if err != nil {
			return nil, errors.Wrap(err, "walk blocks")
		}
	}

	if tr.If("calcGraph") {
		err = c.calcGraph(ctx, p, f)
		if err != nil {
			return nil, errors.Wrap(err, "walk blocks")
		}
	}

	return b, nil
}

func (c *Compiler) walkFunc3(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	//	tr := tlog.SpanFromContext(ctx)

	var e2i, l2i []int

	for i, id := range f.Code {
		x := p.Exprs[id]

		sliceSet(&e2i, id, i)

		switch x := x.(type) {
		case ir.Label:
			sliceSet(&l2i, x, i)
		}
	}

	next := make([][]int, len(f.Code)+1)
	prev := make([][]int, len(f.Code)+1)

	link := func(i, p int) {
		if i < len(f.Code) {
			next[p] = append(next[p], i)
		}

		prev[i] = append(prev[i], p)
	}

	for i, id := range f.Code {
		x := p.Exprs[id]

		switch x := x.(type) {
		case ir.B:
			link(l2i[x.Label], i)
		case ir.BCond:
			link(l2i[x.Label], i)
			link(i+1, i)
		default:
			link(i+1, i)
		}
	}

	//

	fwd := set.MakeBits(0)

	var jobs Jobs
	var path []int

	jobs.Push(Job{})

	for len(jobs) != 0 {
		j := jobs.Pop()
		i := j.i

		path = path[:j.p]

		for i < len(f.Code) {
			if fwd.IsSet(i) {
				break
			}

			fwd.Set(i)

			path = append(path, i)
		}
	}

	return nil
}

func (c *Compiler) walkFunc2(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	tr := tlog.SpanFromContext(ctx)

	var e2i, l2i []int

	for i, id := range f.Code {
		x := p.Exprs[id]

		sliceSet(&e2i, id, i)

		switch x := x.(type) {
		case ir.Label:
			sliceSet(&l2i, x, i)
		}
	}

	next := make([][]int, len(f.Code)+1)
	prev := make([][]int, len(f.Code)+1)

	link := func(i, p int) {
		if i < len(f.Code) {
			next[p] = append(next[p], i)
		}

		prev[i] = append(prev[i], p)
	}

	for i, id := range f.Code {
		x := p.Exprs[id]

		switch x := x.(type) {
		case ir.B:
			link(l2i[x.Label], i)
		case ir.BCond:
			link(l2i[x.Label], i) // first is taken
			link(i+1, i)          // second is pass
		default:
			link(i+1, i)
		}
	}

	//

	bef := make([]BitsInt, len(f.Code))
	dom := make([]BitsInt, len(f.Code))
	choices := make([][]BChoice, len(f.Code))
	cycles := make(map[int][]BitsInt)

	{ // forward pass
		tr := tr.V("walk_fwd")
		tr.Printw("forward pass")

		fwd := set.MakeBits(0)
		merges := map[int][]BChoice{}
		mergen := map[int]int{}

		less := func(i, j int) bool {
			return bef[j].IsSet(i)
		}

		mergeChoices := func(l, r []BChoice) (res []BChoice) {
			if tr.If("merge_choices") {
				defer func() {
					tr.Printw("merge choices", "res", res, "l", l, "r", r)
				}()
			}

			res = make([]BChoice, 0, max(len(l), len(r)))

			i, j := 0, 0
			for i < len(l) && j < len(r) {
				switch {
				case l[i].B == r[j].B:
					res = append(res, BChoice{
						B:     l[i].B,
						Taken: l[i].Taken | r[j].Taken,
					})

					i++
					j++
				case less(l[i].B, r[j].B):
					res = append(res, l[i])
					i++
				default:
					res = append(res, r[j])
					j++
				}
			}

			res = append(res, l[i:]...)
			res = append(res, r[j:]...)

			return res
		}

		var jobs Jobs
		var path []int

		jobs.Push(Job{})

		for len(jobs) != 0 {
			j := jobs.Pop()
			i := j.i

			path = path[:j.p]

			for {
				if fwd.IsSet(i) && cycles[i] != nil {
					for _, p := range prev[i] {
						if !less(i, p) {
							continue
						}
						if !dom[p].IsSet(i) {
							panic(i)
						}

						nodes := bef[p].Copy()
						nodes.Substract(bef[i])
						nodes.Set(i)

						tr.Printw("cycle", "h", i, "b", p, "nodes", nodes)

						cycles[i] = append(cycles[i], nodes)
					}
				}

				if fwd.IsSet(i) {
					break
				}

				if len(prev[i]) > 1 {
					merges[i] = mergeChoices(merges[i], j.choices)
					mergen[i]++
				}

				allPrev := len(prev[i]) <= 1 || mergen[i] == len(prev[i])

				if !allPrev && len(jobs) != 0 {
					break
				}

				if !allPrev {
					cycles[i] = []BitsInt{}
				}

				if len(prev[i]) > 1 {
					j.choices = merges[i]
					delete(merges, i)
					delete(mergen, i)
				}

				fwd.Set(i)

				if len(prev[i]) != 0 {
					p0 := prev[i][0]

					dom[i] = dom[p0].Copy()

					for _, p := range prev[i][1:] {
						dom[i].Intersect(dom[p])
					}

					dom[i].Set(i)

					bef[i].Set(i)

					for _, p := range prev[i] {
						bef[i].Merge(bef[p])
					}
				} else {
					dom[i].Set(i)
					bef[i].Set(i)
				}

				choices[i] = j.choices
				path = append(path, i)

				tr.Printw("walk fwd", "i", i, "next", next[i], "prev", prev[i], "ch", j.choices)

				if len(next[i]) == 0 {
					break
				}

				if len(next[i]) == 1 {
					i = next[i][0]
					continue
				}

				choices[i] = append(choices[i], BChoice{
					B:     i,
					Taken: 1 << 0, // first is taken
				})

				for take, n := range next[i] {
					l := len(j.choices)
					dup := append(j.choices[:l:l], BChoice{
						B:     i,
						Taken: 1 << take,
					})

					jobs.Push(Job{
						i:       n,
						p:       len(path),
						choices: dup,
					})
				}

				break
			}

			tr.Printw("walked path", "st", j.i, "end", i, "prefix", path[:j.p], "path", path[j.p:])
		}

		if len(merges) != 0 {
			panic(merges)
		}
	}

	{ // backward pass
		tr := tr.V("walk_bwd")
		tr.Printw("backward pass")

		fwd := set.MakeBits(0)

		type BNeed struct {
			B    int
			Need BitsInt
		}

		var jobs Jobs
		var phis []ir.Expr
		var perB []BNeed

		var qwe1 func(first, l, r int, sw *df.Switch)
		qwe1 = func(first, l, r int, sw *df.Switch) {
			//	tr.Printw("qwe1", "first", first, "l", l, "r", r)
			if l+1 == r {
				tr.Printw("single choice", "first", first, "choices", choices[perB[l].B], "l", l, "r", r)

				//	if first != len(choices[perB[l].B]) {
				//		panic(first)
				//	}

				ids := make(df.Tuple, len(phis))

				for t, id := range phis {
					ph := p.Exprs[id].(ir.Phi)

					ids[t] = -1

					for _, pp := range ph {
						if pp.B == f.Code[perB[l].B] {
							ids[t] = pp.Expr
							break
						}
					}
				}

				sw.Branches = append(sw.Branches, ids)

				return
			}

			m := l

			for m < r {
				ch1 := choices[perB[m].B]
				if ch1[first].Taken != 1 {
					break
				}

				m++
			}

			if m == l || m == r {
				panic(m)
			}

			branch := choices[perB[m].B][first].B
			bid := f.Code[branch]
			bcond := p.Exprs[bid].(ir.BCond)

			sw.Preds = append(sw.Preds, df.Pred{
				Expr: bcond.Expr,
				Cond: bcond.Cond,
			})

			qwe1(first+1, l, m, sw)
			qwe1(first+1, m, r, sw)

			tr.Printw("switch", "expr", bcond.Expr, "cond", bcond.Cond, "l", l, "m", m, "r", r)
		}

		qwe := func(i int) {
			ch := choices[i]
			first := len(ch)

			for first--; first >= 0; first-- {
				if !dom[i].IsSet(ch[first].B) {
					continue
				}

				break
			}

			tr.Printw("branch point", "choise", first)

			sort.Slice(perB, func(i, j int) bool {
				ch1 := choices[perB[i].B]
				ch2 := choices[perB[j].B]

				for k := first; k < len(ch1) && k < len(ch2); k++ {
					if t1, t2 := ch1[k].Taken, ch2[k].Taken; t1 != t2 {
						return t1 < t2
					}
				}

				return false
			})

			var sw df.Switch

			qwe1(first, 0, len(perB), &sw)

			tr.Printw("qwe", "i", i, "switch", sw)

			i++

			for j := range phis {
				pid := f.Code[i+j]
				phi := p.Exprs[pid].(ir.Phi)

				moved := p.alloc(phi, p.EType[pid])

				tr.Printw("phi-switch rename", "phi", pid, "to", moved)

				if j == 0 {
					p.Exprs[pid] = sw
				} else {
					p.Exprs[pid] = ir.Out(j)
				}
			}
		}

		{
			var need BitsInt

			for _, id := range f.Out {
				i := e2i[id]

				need.Set(i)
			}

			jobs.Push(Job{i: len(f.Code) - 1, need: need})
		}

		for len(jobs) != 0 {
			j := jobs.Pop()
			i := j.i
			need := j.need

			for {
				if fwd.IsSet(i) {
					break
				}

				fwd.Set(i)

				id := f.Code[i]
				x := p.Exprs[id]

				tr.Printw("walk bwd", "i", i, "id", id, "typ", tlog.NextAsType, x, "need", need, "prev", prev[i], "next", next[i])

				needit := need.IsSet(i)
				need.Clear(i)

				if phi, ok := x.(ir.Phi); needit && ok {
					for _, ppid := range phis {
						pp := p.Exprs[ppid].(ir.Phi)

						if len(phi) != len(pp) {
							panic("bad phis")
						}

						for j, pb := range phi {
							if pp[j].B != pb.B {
								panic("bad phis")
							}
						}
					}

					if len(perB) == 0 {
						perB = make([]BNeed, len(phi))
					}

					for j, pb := range phi {
						perB[j].B = e2i[pb.B]
						perB[j].Need.Set(e2i[pb.Expr])
					}

					phis = append(phis, id)

					i--

					continue
				}

				if _, ok := x.(ir.Phi); ok {
					i--

					continue
				}

				if _, ok := x.(ir.Label); ok && len(perB) != 0 {
					for j, l := 0, len(phis)-1; j < len(phis); j++ { // they were added in reverse order, preserve it
						phis[j], phis[l-j] = phis[l-j], phis[j]
					}

					tr.Printw("merge", "i", i, "phis", phis, "ch", choices[i], "dom", dom[i])

					for _, pb := range perB {
						tr.Printw("b from", "b", pb.B, "ch", choices[pb.B], "need", pb.Need)
					}

					if len(perB) > 1 {
						qwe(i)
					} else {
						for _, pid := range phis {
							phi := p.Exprs[pid].(ir.Phi)

							p.Exprs[pid] = df.Alias(phi[0].Expr)
						}
					}

					//

					for _, pb := range perB {
						jobs.Push(Job{
							i:    pb.B,
							need: pb.Need,
						})
					}

					phis = phis[:0]
					perB = perB[:0]

					break
				}

				if needit {
					switch x := x.(type) {
					case ir.Imm, ir.Args:
					case ir.Cmp:
						need.SetAll(e2i[x.L], e2i[x.R])
					case ir.Add:
						need.SetAll(e2i[x.L], e2i[x.R])
					case ir.Sub:
						need.SetAll(e2i[x.L], e2i[x.R])
					default:
						panic(x)
					}
				}

				if need.Size() == 0 {
					break
				}

				if len(prev[i]) == 0 {
					break
				}

				if len(prev[i]) == 1 {
					i = prev[i][0]
					continue
				}

				for _, n := range prev[i] {
					jobs.Push(Job{
						i: n,
						//	p:       len(path),
						//	choices: dup,
					})
				}

				break
			}
		}
	}

	return nil
}

func (c *Compiler) walkBlock(ctx context.Context, p *pkgContext, start int, next, prev [][]int, cb func(i int, j Job)) (err error) {
	tr := tlog.SpanFromContext(ctx)

	fwd := set.MakeBits(0)
	//	dom := make([]BitsInt, len(next))
	bef := make([]BitsInt, len(next))
	merges := map[int][]BChoice{}

	less := func(i, j int) bool {
		return bef[j].IsSet(i)
	}

	mergeChoices := func(l, r []BChoice) (res []BChoice) {
		defer func() {
			tr.Printw("merge choices", "res", res, "l", l, "r", r)
		}()

		res = make([]BChoice, 0, max(len(l), len(r)))

		i, j := 0, 0
		for i < len(l) && j < len(r) {
			switch {
			case l[i].B == r[j].B:
				res = append(res, BChoice{
					B:     l[i].B,
					Taken: l[i].Taken | r[j].Taken,
				})

				i++
				j++
			case less(l[i].B, r[j].B):
				res = append(res, l[i])
				i++
			default:
				res = append(res, r[j])
				j++
			}
		}

		res = append(res, l[i:]...)
		res = append(res, r[j:]...)

		return res
	}

	var jobs Jobs
	var path []int

	jobs.Push(Job{i: start})

	for len(jobs) != 0 {
		j := jobs.Pop()
		i := j.i

		for _, i := range path[:j.p] {
			_ = i
		}

		path = path[:j.p]

		for {
			if fwd.IsSet(i) {
				break
			}

			if len(prev[i]) > 1 {
				merges[i] = mergeChoices(merges[i], j.choices)
			}

			allPrev := true

			for _, p := range prev[i] {
				allPrev = allPrev && fwd.IsSet(p)
			}

			if !allPrev && len(jobs) != 0 {
				break
			}

			if len(prev[i]) > 1 {
				j.choices = merges[i]
				delete(merges, i)
			}

			fwd.Set(i)

			if len(prev[i]) != 0 {
				p0 := prev[i][0]

				//	dom[i] = dom[p0].Copy()
				//	dom[i].Set(p0)

				//	for _, p := range prev[i][1:] {
				//		dom[i].Intersect(dom[p])
				//	}

				bef[i] = bef[p0].Copy()
				bef[i].Set(p0)

				for _, p := range prev[i][1:] {
					bef[i].Merge(bef[p])
				}
			}

			path = append(path, i)
			cb(i, j)

			tr.Printw("walk", "i", i, "next", next[i], "prev", prev[i], "ch", j.choices)

			if len(next[i]) == 0 {
				break
			}

			if len(next[i]) == 1 {
				i = next[i][0]
				continue
			}

			for take, n := range next[i] {
				l := len(j.choices)
				dup := append(j.choices[:l:l], BChoice{
					B:     i,
					Taken: 1 << take,
				})

				jobs.Push(Job{
					i:       n,
					p:       len(path),
					choices: dup,
				})
			}

			break
		}

		tr.Printw("walked path", "st", j.i, "end", i, "prefix", path[:j.p], "path", path[j.p:])
	}

	if len(merges) != 0 {
		panic(merges)
	}

	return nil
}

func (c *Compiler) walkFunc1(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	tr := tlog.SpanFromContext(ctx)
	//	base := f.Code[0]

	var e2i, l2i []int

	for i, id := range f.Code {
		x := p.Exprs[id]

		sliceSet(&e2i, id, i)

		switch x := x.(type) {
		case ir.Label:
			sliceSet(&l2i, x, i)
		}
	}

	prev := make([][]int, len(f.Code))

	link := func(i, p int) {
		if i == len(f.Code) {
			return
		}

		prev[i] = append(prev[i], p)
	}

	for i, id := range f.Code {
		x := p.Exprs[id]

		switch x := x.(type) {
		case ir.B:
			link(l2i[x.Label], i)
		case ir.BCond:
			link(i+1, i)
			link(l2i[x.Label], i)
		default:
			link(i+1, i)
		}
	}

	//

	fwd := set.MakeBits(0)
	bwd := set.MakeBits(0)

	backup := make([]int, len(f.Code))
	cameBy := make([]BitsInt, len(f.Code))
	choices := make([][]BChoice, len(f.Code))
	merges := make(map[int][]int)
	cycles := make(map[int][]BitsInt)

	var jobs Jobs
	var path []int

	commonOrigin := func(l, r int) (o int) {
		x := cameBy[l].Copy()
		x.Intersect(cameBy[r])

		for j := len(path) - 1; j >= 0; j-- {
			i := path[j]
			if x.IsSet(i) {
				o = i
				break
			}
		}

		tr.V("commonOrigin").Printw("common orig", "o", o, "l", l, "r", r, "x", x, "l", cameBy[l], "r", cameBy[r], "path", path)
		tr.V("commonOrigin").Printw("choices", "choices_l", choices[l], "choices_r", choices[r])

		return o
	}

	findCycle := func(h, i int) (r BitsInt) {
		r.Set(h)
		r.Set(i)

		q := []int{i}

		for j := 0; j < len(q); j++ {
			for _, p := range prev[q[j]] {
				if r.IsSet(p) {
					continue
				}

				r.Set(p)
				q = append(q, p)
			}
		}

		tr.V("findCycle").Printw("find cycle", "h", h, "i", i, "nodes", r)

		return
	}

	jobs.Push(Job{})

	for len(jobs) != 0 {
		j := jobs.Pop()
		i := j.i

		tr.Printw("job start", "st", i, "choices", j.choices)

		for _, i := range path[j.p:] {
			bwd.Set(i)
		}

		path = path[:j.p]

		for i < len(f.Code) {
			visited := fwd.Copy()
			visited.Substract(bwd)
			visited.Set(i)
			visited.Strip()

			cameBy[i].Merge(visited)

			if fwd.IsSet(i) || bwd.IsSet(i) {
				break
			}

			id := f.Code[i]
			x := p.Exprs[id]

			if l := len(path); l != 0 {
				backup[i] = path[l-1]
			}

			fwd.Set(i)
			path = append(path, i)
			choices[i] = j.choices

			switch x := x.(type) {
			case ir.B:
				i = l2i[x.Label]
			case ir.BCond:
				dup := append(j.choices[:cap(j.choices)], BChoice{B: i, Taken: bTaken})
				j.choices = append(j.choices, BChoice{B: i, Taken: bSkipped})

				jobs.Push(Job{i: l2i[x.Label], p: len(path), choices: dup})

				i++
			default:
				i++
			}
		}

		var node walked

		switch {
		case i == len(f.Code):
			node = walkEnd
		case bwd.IsSet(i):
			node = walkMerge

			merges[i] = append(merges[i], commonOrigin(
				path[len(path)-1],
				backup[i],
			))
		case fwd.IsSet(i):
			node = walkCycle

			r := findCycle(i, path[len(path)-1])

			cycles[i] = append(cycles[i], r)
		default:
			panic(i)
		}

		tr.Printw("walked", "st", j.i, "end", i, "stop", node, "prefix", path[:j.p], "path", path[j.p:])
		tr.V("walk_sets").Printw("walk sets", "fwd", fwd)
		tr.V("walk_sets").Printw("walk sets", "bwd", bwd)
	}

	tr.Printw("choices", "choices", choices)
	tr.Printw("merges", "merges", merges)
	for h, c := range cycles {
		tr.Printw("cycles", "h", h, "cycle", c)
	}

	//

	fwd.Reset()
	bwd.Reset()

	type BNeed struct {
		b    ir.Expr
		need BitsInt
	}

	var need BitsInt
	var phis []ir.Expr
	var perB []BNeed

	rename := map[ir.Expr]ir.Expr{}

	setNeed := func(ids ...ir.Expr) {
		for _, id := range ids {
			need.Set(e2i[id])
		}
	}

	addPerB := func(b, id ir.Expr) {
		i := e2i[id]

		for j := range perB {
			if perB[j].b != b {
				continue
			}

			perB[j].need.Set(i)
			return
		}

		var bneed BitsInt

		bneed.Set(i)

		perB = append(perB, BNeed{
			b:    b,
			need: bneed,
		})
	}

	for _, id := range f.Out {
		need.Set(e2i[id])
	}

	bjobs := make(map[int]Job)

	jobs.Push(Job{i: len(f.Code) - 1, need: need})

	for len(jobs) != 0 {
		j := jobs.Pop()
		i := j.i
		need = j.need

		tr.Printw("job", "st", i, "need", need)

		for i >= 0 {
			if bwd.IsSet(i) {
				j, ok := bjobs[i]
				if !ok {
					panic(i)
				}

				delete(bjobs, i)

				need.Clear(i)
				j.need.Merge(need)

				jobs.Push(j)

				tr.Printw("merged branch job", "st", j.i, "need", j.need)

				break
			}

			bwd.Set(i)

			id := f.Code[i]
			x := p.Exprs[id]

			tr.Printw("analyze", "i", i, "id", id, "typ", tlog.NextAsType, x, "needit", need.IsSet(i), "need", need, "prev", prev[i], "phis", phis)

			needit := need.IsSet(i)
			need.Clear(i)

			if _, ok := x.(ir.Phi); needit && ok {
				phis = append(phis, id)

				//	for _, pb := range p {
				//		addPerB(pb.B, pb.Expr)
				//	}

				i--

				continue
			}

			if _, ok := x.(ir.Label); ok && len(phis) != 0 {
				if len(merges[i]) != 0 {
					tr.Printw("switch", "i", i, "id", id, "merges", merges[i])

					sid := p.id()
					swtch := &df.Switch{}

					for out, pid := range phis {
						phi := p.Exprs[pid].(ir.Phi)

						tr.Printw("switch phi", "id", pid, "phi", phi)

						id := p.alloc(df.Out(out), p.EType[pid])
						rename[pid] = id
					}

					p.Exprs[sid] = swtch
				}

				for _, bcond := range merges[i] {
					need.Set(bcond)
				}

				for _, pid := range phis {
					p := p.Exprs[pid].(ir.Phi)

					for _, pb := range p {
						addPerB(pb.B, pb.Expr)
					}
				}

				for _, pb := range perB {
					pb.need.Merge(need)
					jobs.Push(Job{i: e2i[pb.b], need: pb.need})

					tr.Printw("add job", "st", e2i[pb.b], "need", pb.need, "i", i, "b", pb.b)
				}

				phis = phis[:0]
				perB = perB[:0]

				break
			}

			if x, ok := x.(ir.BCond); needit && ok {
				setNeed(x.Expr)

				bjobs[i] = Job{i: i - 1, need: need}

				tr.Printw("add branch job", "st", i-1, "need", need)

				break
			}

			if needit {
				switch x := x.(type) {
				case ir.Imm, ir.Args:
				case ir.Cmp:
					setNeed(x.L, x.R)
				case ir.Add:
					setNeed(x.L, x.R)
				default:
					panic(x)
				}
			}

			if need.Size() == 0 {
				break
			}

			if len(prev[i]) != 1 {
				panic(i)
			}

			i = prev[i][0]
		}

		tr.Printw("job done", "st", j.i, "end", i, "need", need)
	}

	if len(bjobs) != 0 {
		panic(bjobs)
	}

	if len(perB) != 0 {
		panic(perB)
	}
	if len(phis) != 0 {
		panic(len(phis))
	}

	return nil
}

func (c *Compiler) walkFunc0(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	tr := tlog.SpanFromContext(ctx)

	e2i := make(map[ir.Expr]int, len(f.Code))
	l2i := make([]int, 0, 8)
	lin := make([][]int, 0, 8)

	setLabel := func(l ir.Label, i int) {
		for len(l2i) <= int(l) {
			l2i = append(l2i, -1)
		}

		l2i[l] = i
	}

	setLabelIn := func(l ir.Label, i int) {
		for len(lin) <= int(l) {
			lin = append(lin, nil)
		}

		lin[l] = append(lin[l], i)
	}

	for i, id := range f.Code {
		x := p.Exprs[id]

		e2i[id] = i

		switch x := x.(type) {
		case ir.Label:
			setLabel(x, i)
		case ir.B:
			setLabelIn(x.Label, i)
		case ir.BCond:
			setLabelIn(x.Label, i)
		}
	}

	next := make([][]int, len(f.Code)+1)
	prev := make([][]int, len(f.Code)+1)

	link := func(i, j int) {
		next[i] = append(next[i], j)
		prev[j] = append(prev[j], i)
	}

	for i, id := range f.Code {
		x := p.Exprs[id]

		switch x := x.(type) {
		case ir.B:
			link(i, l2i[x.Label])
		case ir.BCond:
			link(i, i+1)
			link(i, l2i[x.Label])
		default:
			link(i, i+1)
		}
	}

	//

	findLoop := func(h, i int) *set.Bitmap {
		res := set.NewBitmap(0)

		res.Set(h)
		res.Set(i)

		q := []int{i}

		for j := 0; j < len(q); j++ {
			for _, p := range prev[q[j]] {
				if res.IsSet(p) {
					continue
				}

				res.Set(p)
				q = append(q, p)
			}
		}

		return res
	}

	reachableFrom := make([]set.Bitmap, len(f.Code)+1)
	pathSet := set.MakeBitmap(0)

	visit := make([]walked, len(f.Code)+1)
	visit[len(f.Code)] = walkEnd

	path := make([]int, 0, len(f.Code))
	jobs := Jobs{{i: 0}}

	commonOrigin := func(x set.Bitmap) int {
		for j := len(path) - 1; j >= 0; j-- {
			i := path[j]

			if !x.IsSet(i) {
				continue
			}

			return i
		}

		return -1
	}

	loops := map[[2]int]*set.Bitmap{}
	merges := map[int]map[int]int{}

	findSwitch3 := func(m, p int) {
		for _, p1 := range prev[m] {
			if p == p1 {
				continue
			}

			x := reachableFrom[p1]

			if x.Size() == 0 {
				continue
			}

			x = x.AndCopy(reachableFrom[p])
			o := commonOrigin(x)

			tr.Printw("find switch 3", "m", m, "p", p, "p1", p1, "o", o)
			//	tr.Printw("reachables", "p", reachableFrom[p], "p1", reachableFrom[p1])

			if merges[m] == nil {
				merges[m] = map[int]int{}
			}

			merges[m][o]++
		}
	}

	for len(jobs) != 0 {
		job := jobs.Pop()
		i := job.i

		for j := job.p; j < len(path); j++ {
			visit[path[j]] = walkMerge
			pathSet.Clear(path[j])
		}

		path = path[:job.p]

		//	tr.Printw("pathSet", "i", i, "pathSet", pathSet, "path", path)

		for i < len(f.Code) {
			pathSet.Set(i)
			reachableFrom[i].Or(pathSet)

			if visit[i] != walkNone {
				break
			}

			visit[i] = walkCycle
			path = append(path, i)

			for _, j := range next[i][1:] {
				jobs.Push(Job{i: j, p: len(path)})
			}

			i = next[i][0]
		}

		{
			lab := ir.Label(-1)
			if i < len(f.Code) {
				lab = p.Exprs[f.Code[i]].(ir.Label)
			}

			var phis []ir.Expr
			philen := 0

			for j := i + 1; j < len(f.Code); j++ {
				id := f.Code[j]
				x := p.Exprs[id]

				phi, ok := x.(ir.Phi)
				if !ok {
					break
				}

				phis = append(phis, id)

				if philen != 0 && philen != len(phi) {
					panic(id)
				}

				philen = len(phi)
			}

			tr.Printw("path walked", "st", job.i, "end", i, "visit", visit[i], "label", lab, "ins", philen, "phis", phis)
			tr.Printw("path walked", "prefix", path[:job.p], "path", path[job.p:])
		}

		switch visit[i] {
		case walkEnd:
		case walkCycle:
			id := [2]int{i, path[len(path)-1]}
			loops[id] = findLoop(id[0], id[1])
		case walkMerge:
			findSwitch3(i, path[len(path)-1])
		default:
			panic(visit[i])
		}
	}

	p.loop = make([]int, len(f.Code))
	loopsIn := make([][][2]int, len(f.Code))

	for i := range f.Code {
		var ls [][2]int

		for k, l := range loops {
			if l.IsSet(i) {
				ls = append(ls, k)
			}
		}

		sort.Slice(ls, func(i, j int) bool {
			return loops[ls[i]].Size() > loops[ls[j]].Size()
		})

		loopsIn[i] = ls
		p.loop[i] = len(ls)
	}

	tr.Printw("func structure")

	for i, id := range f.Code {
		x := p.Exprs[id]

		args := []interface{}{"i", i, "id", id /*"tp", p.EType[id], */, "typ", tlog.NextAsType, x, "val", x}

		if l, ok := x.(ir.Label); ok {
			args = append(args, "l_in", lin[l])
		}

		var ls [][2]int

		for k, l := range loops {
			if l.IsSet(i) {
				ls = append(ls, k)
			}
		}

		sort.Slice(ls, func(i, j int) bool {
			return loops[ls[i]].Size() > loops[ls[j]].Size()
		})

		if len(ls) != 0 {
			args = append(args, "loops", ls)
		}

		if v, ok := merges[i]; ok {
			args = append(args, "merge", v)
		}

		tr.Printw("code", args...)
	}

	//

	for i := range visit {
		visit[i] = walkNone
	}

	//	var curLoopsIn [][2]int
	path = path[:0]
	jobs = jobs[:0]

	return nil
}

func (c *Compiler) findLoops(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	tr := tlog.SpanFromContext(ctx)

	i2b := make([]int, len(f.Code))

	l2i := make(map[ir.Label]int)
	lin := make(map[ir.Label][]int)

	bb := 0
	next := false

	for i, id := range f.Code {
		x := p.Exprs[id]

		if bb == 0 {
			switch x.(type) {
			case ir.Args, ir.Out:
			}
		}

		if l, ok := x.(ir.Label); ok || next {
			bb++
			next = false

			if ok {
				l2i[l] = i
			}
		}

		i2b[i] = bb

		bfun := func(l ir.Label) {
			next = true
			lin[l] = append(lin[l], i)
		}

		switch x := x.(type) {
		case ir.B:
			bfun(x.Label)
		case ir.BCond:
			bfun(x.Label)
		}
	}

	prev := make([][]int, len(f.Code))

	for i, id := range f.Code {
		x := p.Exprs[id]

		link := func(d, s int) {
			if d == len(f.Code) {
				return
			}

			prev[d] = append(prev[d], s)
		}

		switch x := x.(type) {
		case ir.B:
			link(l2i[x.Label], i)
		case ir.BCond:
			link(l2i[x.Label], i)
			link(i+1, i)
		default:
			link(i+1, i)
		}
	}

	tr.Printw("prev", "prev", prev)

	loop := make([]int, len(f.Code))
	p.loop = loop

	visit := make([]int, len(f.Code))
	path := make([]int, 0, len(f.Code))

	type il struct{ i, l int }
	stack := []il{{}}

	var backbone [][2]int // edge direction: <-

	for len(stack) != 0 {
		// pop
		l := len(stack) - 1
		start := stack[l]
		stack = stack[:l]

		for j := start.l; j < len(path); j++ {
			visit[path[j]] = 2
		}

		i := start.i
		path = path[:start.l]

		tr.Printw("restart", "i", i)

		for i < len(f.Code) {
			id := f.Code[i]
			x := p.Exprs[id]

			if visit[i] == 1 {
				tr.Printw("loop", "orig", i, "dst", path[len(path)-1])
				backbone = append(backbone, [2]int{i, path[len(path)-1]})
				break
			}
			if visit[i] == 2 {
				break
			}

			path = append(path, i)
			visit[i] = 1
			loop[i]++

			switch x := x.(type) {
			case ir.B:
				i = l2i[x.Label]
			case ir.BCond:
				stack = append(stack, il{i: i + 1, l: len(path)})
				i = l2i[x.Label]
			default:
				i++
			}
		}

		tr.Printw("walked to end", "i", i, "walked", loop)
	}

	add := func(i int) {
		if visit[i] != 0 {
			return
		}

		visit[i] = 1
		loop[i]++
	}

	for _, back := range backbone {
		path = path[:0]

		for i := range visit {
			visit[i] = 0
		}

		add(back[0])
		add(back[1])

		path = append(path, back[1])

		for len(path) != 0 {
			l := len(path)
			cur := path[l-1]
			path = path[:l-1]

			for _, p := range prev[cur] {
				if visit[p] != 0 {
					continue
				}

				path = append(path, p)
				add(p)
			}
		}

		tr.Printw("loop nodes", "nodes", visit)
	}

	if tr.If("dump_func_struct") {
		tr.Printw("func structure")

		for i, id := range f.Code {
			x := p.Exprs[id]

			args := []interface{}{"i", i, "bb", i2b[i], "loop", loop[i], "id", id /*"tp", p.EType[id], */, "typ", tlog.NextAsType, x, "val", x}

			if l, ok := x.(ir.Label); ok {
				args = append(args, "l_in", lin[l])
			}

			tr.Printw("code", args...)
		}
	}

	return nil
}

func (c *Compiler) calcGraph(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	tr := tlog.SpanFromContext(ctx)

	p.slots = make(map[ir.Expr]*set.Bitmap, len(f.Code))
	labelhave := make(map[ir.Label]*set.Bitmap)
	labelneed := make(map[ir.Label]map[ir.Expr]*set.Bitmap) // B -> slots

	for _, id := range f.Code {
		switch x := p.Exprs[id].(type) {
		case ir.Label:
			labelhave[x] = set.NewBitmap(0)
			labelneed[x] = make(map[ir.Expr]*set.Bitmap)
		}
	}

	vals := set.MakeBitmap(len(f.Code))

	setID := func(ids ...ir.Expr) {
		for _, id := range ids {
			vals.Set(int(id))
		}
	}

	// forward pass

	for pass := 0; pass < 2; pass++ {
		vals.Reset()

		for i, id := range f.Code {
			x := p.Exprs[id]

			setID(id)

			switch x := x.(type) {
			case ir.Imm, ir.Args, ir.Out /*, ir.State, ir.Effect*/ :
			case ir.Cmp, ir.Add, ir.Sub, ir.Mul, ir.Div, ir.Mod:
			case ir.BitAnd, ir.BitOr:
				//	case ir.Ptr, ir.Load, ir.Offset:
				//	case ir.Store:
				//	case ir.Alloc:
			case ir.Label:
				vals.Clear(int(id))
				vals.Or(*labelhave[x])

				for j := i + 1; j < len(f.Code); j++ {
					x, ok := p.Exprs[id].(ir.Phi)
					if !ok {
						break
					}

					for _, xx := range x {
						vals.Clear(int(xx.Expr))
					}
				}

				for j := i + 1; j < len(f.Code); j++ {
					_, ok := p.Exprs[id].(ir.Phi)
					if !ok {
						break
					}

					setID(id)
				}
			case ir.B:
				vals.Clear(int(id))
				labelhave[x.Label].Or(vals)
			case ir.BCond:
				vals.Clear(int(id))
				labelhave[x.Label].Or(vals)
			case ir.Phi:
			//	for _, xx := range x {
			//		vals.Clear(int(xx.Expr))
			//	}

			//	setID(id)
			case ir.Call:
				// TODO
				//	case tp.Array, tp.Int:
			default:
				panic(x)
			}

			if pass != 0 {
				p.slots[id] = vals.CopyPtr()
			}

			switch x.(type) {
			case ir.B:
				vals.Reset()
			}
		}
	}

	// backward pass

	for _, vals := range labelhave {
		vals.Reset()
	}

	var phis map[ir.Expr]*set.Bitmap

	for pass := 0; pass < 2; pass++ {
		vals.Reset()

		for i := len(f.Code) - 1; i >= 0; i-- {
			id := f.Code[i]
			x := p.Exprs[id]

			tr := tr.V("backward_pass")
			tr.Printw("backward pass", "pass", pass, "i", i, "id", id, "typ", tlog.NextAsType, x, "vals", vals, "phis", phis)

			switch x := x.(type) {
			case ir.B:
				vals.Reset()

				tr.Printw("backward pass", "labelhave", labelhave[x.Label], "labelneed", labelneed[x.Label])
				tr.Printw("reset from label", "label", x.Label, "b", id, "vals", labelneed[x.Label][id])

				vals.Or(*labelhave[x.Label])

				if vv, ok := labelneed[x.Label][id]; ok {
					vals.Or(*vv)
				}
			case ir.BCond:
				tr.Printw("backward pass", "labelhave", labelhave[x.Label], "labelneed", labelneed[x.Label])
				tr.Printw("add from label", "label", x.Label, "b", id, "vals", labelneed[x.Label][id])

				vals.Or(*labelhave[x.Label])

				if vv, ok := labelneed[x.Label][id]; ok {
					vals.Or(*vv)
				}
			}

			if pass != 0 {
				switch x.(type) {
				case ir.Label, ir.Phi:
				default:
					p.slots[id].And(vals)
				}
			}

			vals.Clear(int(id))

			switch x := x.(type) {
			case ir.Imm, ir.Args, ir.Out /*, ir.State, ir.Effect*/ :
				//	case ir.Ptr, ir.Load, ir.Offset:
				//	case ir.Store:
				//	case ir.Alloc:
			case ir.Label:
				labelhave[x].Reset()
				labelhave[x].Or(vals)

				labelneed[x] = phis
				phis = nil
			case ir.B:
			case ir.BCond:
				setID(x.Expr)
			case ir.Phi:
				if phis == nil {
					phis = make(map[ir.Expr]*set.Bitmap)
				}

				for _, xx := range x {
					//	setID(xx.Expr)
					if _, ok := phis[xx.B]; !ok {
						phis[xx.B] = set.NewBitmap(len(f.Code))
					}

					phis[xx.B].Set(int(xx.Expr))
				}
			case ir.Call:
				setID(x.In...)
			//	vals.Set(int(x.StateIn))
			//	vals.Set(int(x.EffectIn))
			//	case tp.Array, tp.Int:
			case ir.Cmp:
				setID(x.L, x.R)
			case ir.Add:
				setID(x.L, x.R)
			case ir.Sub:
				setID(x.L, x.R)
			case ir.Mul:
				setID(x.L, x.R)
			case ir.Div:
				setID(x.L, x.R)
			case ir.Mod:
				setID(x.L, x.R)
			case ir.BitAnd:
				setID(x.L, x.R)
			case ir.BitOr:
				setID(x.L, x.R)
			default:
				panic(x)
			}
		}
	}

	// phi run

	for i, id := range f.Code {
		x := p.Exprs[id]

		_, ok := x.(ir.Label)
		if !ok {
			continue
		}

		vals.Reset()

		j := i + 1
		for ; j < len(f.Code); j++ {
			jid := f.Code[j]
			x := p.Exprs[jid]

			_, ok := x.(ir.Phi)
			if !ok {
				break
			}

			setID(jid)
		}

		for k := i; k < j; k++ {
			p.slots[f.Code[k]].Reset()
			p.slots[f.Code[k]].Or(vals)
		}
	}

	if tr.If("dump_func_slots") {
		tr.Printw("func slots")

		for i, id := range f.Code {
			x := p.Exprs[id]

			tr.Printw("code", "loop", p.loop[i], "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x, "slots", p.slots[id])
		}
	}

	p.edges = make(map[ir.Expr]*set.Bitmap)
	clique := 0

	for _, id := range f.Code {
		x := p.Exprs[id]

		switch x.(type) {
		case ir.Label, ir.B, ir.BCond:
			continue
		}

		if c := p.slots[id].Size(); c > clique {
			clique = c
		}

		p.edges[id] = set.NewBitmap(len(f.Code))

		p.slots[id].Range(func(xi int) bool {
			x := ir.Expr(xi)

			if x == id {
				return true
			}

			if _, ok := p.edges[x]; !ok {
				p.edges[x] = set.NewBitmap(len(f.Code))
			}

			p.edges[id].Set(xi)
			p.edges[x].Set(int(id))

			return true
		})
	}

	if tr.If("dump_func_graph") {
		tr.Printw("graph", "clique", clique)

		for _, id := range f.Code {
			if p.edges[id].Size() == 0 {
				continue
			}

			tr.Printw("edges", "id", id, "x", p.edges[id])
		}
	}

	return nil
}

func (p *pkgContext) id() ir.Expr {
	return ir.Expr(len(p.Exprs))
}

func (p *pkgContext) alloc(x any, tp ir.Type) ir.Expr {
	id := p.id()
	p.Exprs = append(p.Exprs, x)
	p.EType = append(p.EType, tp)

	return id
}

func (jobs *Jobs) Pop() Job {
	last := len(*jobs) - 1

	top := (*jobs)[last]
	*jobs = (*jobs)[:last]

	return top
}

func (jobs *Jobs) Push(j Job) {
	*jobs = append(*jobs, j)
}

func (w walked) String() string {
	switch w {
	case walkNone:
		return "0"
	case walkCycle:
		return "cycle"
	case walkMerge:
		return "merge"
	case walkEnd:
		return "end"
	default:
		return fmt.Sprintf("%d", int(w))
	}
}

func (c BChoice) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	b = e.AppendMap(b, 1)
	b = e.AppendInt(b, int(c.B))
	b = e.AppendSemantic(b, tlwire.Hex)
	b = e.AppendInt(b, int(c.Taken))

	return b
}
