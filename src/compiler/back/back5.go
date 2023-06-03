package back

import (
	"context"
	"fmt"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/slowlang/slow/src/compiler/bitmap"
	"github.com/slowlang/slow/src/compiler/ir"
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
		slots map[ir.Expr]*bitmap.Big
		edges map[ir.Expr]*bitmap.Big
	}

	Reg int
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

	BL	_main

	LDP     FP, LR, [SP], #16
	RET
`, pkg.Path)

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

		for _, id := range f.Code {
			x := p.Exprs[id]

			tr.Printw("code", "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}
	}

	if tr.If("stop_after_dump") {
		return b, nil
	}

	err = c.findLoops(ctx, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "walk blocks")
	}

	if tr.If("stop_after_loops") {
		return b, nil
	}

	err = c.calcGraph(ctx, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "walk blocks")
	}

	if tr.If("stop_after_graph") {
		return b, nil
	}

	return b, nil
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

		tr.Printw("done", "i", i, "loop_cur", loop)
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

	p.slots = make(map[ir.Expr]*bitmap.Big, len(f.Code))
	labelhave := make(map[ir.Label]*bitmap.Big)
	labelneed := make(map[ir.Label]map[ir.Expr]*bitmap.Big) // B -> slots

	for _, id := range f.Code {
		switch x := p.Exprs[id].(type) {
		case ir.Label:
			labelhave[x] = bitmap.New()
			labelneed[x] = make(map[ir.Expr]*bitmap.Big)
		}
	}

	vals := bitmap.Make()

	set := func(ids ...ir.Expr) {
		for _, id := range ids {
			vals.Set(int(id))
		}
	}

	// forward pass

	for pass := 0; pass < 2; pass++ {
		vals.Reset()

		for i, id := range f.Code {
			x := p.Exprs[id]

			set(id)

			switch x := x.(type) {
			case ir.Imm, ir.Args, ir.Out /*, ir.State, ir.Effect*/ :
			case ir.Cmp, ir.Add, ir.Sub, ir.Mul, ir.Div, ir.Mod:
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

					set(id)
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

			//	set(id)
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

	var phis map[ir.Expr]*bitmap.Big

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
			case ir.Cmp:
				set(x.L, x.R)
			case ir.Add:
				set(x.L, x.R)
			case ir.Sub:
				set(x.L, x.R)
			case ir.Mul:
				set(x.L, x.R)
			case ir.Div:
				set(x.L, x.R)
			case ir.Mod:
				set(x.L, x.R)
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
				set(x.Expr)
			case ir.Phi:
				if phis == nil {
					phis = make(map[ir.Expr]*bitmap.Big)
				}

				for _, xx := range x {
					//	set(xx.Expr)
					if _, ok := phis[xx.B]; !ok {
						phis[xx.B] = bitmap.New()
					}

					phis[xx.B].Set(int(xx.Expr))
				}
			case ir.Call:
				set(x.In...)
			//	vals.Set(int(x.StateIn))
			//	vals.Set(int(x.EffectIn))
			//	case tp.Array, tp.Int:
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

			set(jid)
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

	p.edges = make(map[ir.Expr]*bitmap.Big)
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

		p.edges[id] = bitmap.New()

		p.slots[id].Range(func(xi int) bool {
			x := ir.Expr(xi)

			if x == id {
				return true
			}

			if _, ok := p.edges[x]; !ok {
				p.edges[x] = bitmap.New()
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
