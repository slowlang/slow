package back

import (
	"context"
	"fmt"

	"github.com/slowlang/slow/src/compiler/df"
	"github.com/slowlang/slow/src/compiler/ir"
	"nikand.dev/go/heap"
	"tlog.app/go/errors"
	"tlog.app/go/loc"
	"tlog.app/go/tlog"
	"tlog.app/go/tlog/tlwire"
)

type (
	Arch interface{}

	Compiler struct{}

	pkgContext struct {
		Arch
		*ir.Package

		*funContext

		calleeSaved Reg
	}

	funContext struct {
		*ir.Func

		dom []BitsInt
	}

	Reg int
	Mov struct{ W, R Reg }

	job struct {
		st   int
		wait int
		//	path path
	}

	jobs struct {
		heap.Heap[job]
		path path
	}

	path []df.Pred
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

	for _, fid := range p.Funcs {
		f := p.Exprs[fid].(*ir.Func)

		b = append(b, '\n')

		b, err = c.compileFunc(ctx, b, p, f)
		if err != nil {
			return nil, errors.Wrap(err, "func %v", f.Name)
		}
	}

	if tr.If("omit_out") {
		b = nil
	}

	return b, nil
}

func (c *Compiler) compileFunc(ctx context.Context, b []byte, p *pkgContext, fn *ir.Func) (_ []byte, err error) {
	tr, ctx := tlog.SpawnFromContextAndWrap(ctx, "compile func", "name", fn.Name, "in", fn.In, "out", fn.Out)
	defer tr.Finish("err", &err)

	idx := func(i int) (ir.Expr, any) {
		id := fn.Code[i]
		x := p.Exprs[id]

		return id, x
	}

	f := &funContext{Func: fn}

	if tr.If("dump_func_before") {
		for i, id := range f.Code {
			x := p.Exprs[id]

			tr.Printw("code before", "i", i, "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}
	}

	{
		labins := map[ir.Label]int{}
		l2i := map[ir.Label]int{}

		for i, id := range f.Code {
			x := p.Exprs[id]

			switch x := x.(type) {
			case ir.Label:
				l2i[x] = i
			case ir.B:
				labins[x.Label]++
			case ir.BCond:
				labins[x.Label]++
			}
		}

		bpath := map[ir.Expr]path{} // ir.B -> []path
		lpath := map[ir.Label]path{}

		lmerge := func(lab ir.Label, p path) {
			p0, ok := lpath[lab]
			if !ok {
				lpath[lab] = p
				return
			}

			i := 0

			if tlog.If("lmerge0") {
				for i < len(p0) && i < len(p) && p0[i] == p[i] {
					i++
				}
			} else {
				p0 = dup(p0)

				for i < len(p0) && i < len(p) && p0[i].Expr() == p[i].Expr() {
					p0[i] |= p[i] & 0b11
					i++
				}
			}

			lpath[lab] = p0[:i]
		}

		jobs := jobs{Heap: heap.Heap[job]{Less: jobsLess}}

		jobs.Push(job{st: 0})

		for jobs.Len() != 0 {
			j := jobs.Pop()
			i := j.st

			var path path

			for i < len(f.Code) {
				id, x := idx(i)

				if l, ok := x.(ir.Label); ok {
					path = lpath[l]

					_, x2 := idx(i + 1)

					p, ok := x2.(ir.Phi)
					if !ok {
						i++
						continue
					}

					tr.V("job_lab").Printw("label merge", "i", i, "id", id, "lab", l, "pref", path, "phi", p)

					for _, pb := range p {
						tr.V("job_lab").Printw("label input", "expr", pb.Expr, "b", pb.B, "path", bpath[pb.B])
					}

					i++
					continue
				}

				if b, ok := x.(ir.B); ok {
					bpath[id] = path
					lmerge(b.Label, path)
					jobs.Push(job{st: l2i[b.Label], wait: labins[b.Label]})
					break
				}

				b, ok := x.(ir.BCond)
				if !ok {
					i++
					continue
				}

				l := len(path)
				path = path[:l:l]

				{
					path := append(path, df.ToPred(b.Expr, df.PredHeld))

					bpath[id] = path
					lmerge(b.Label, path)
					jobs.Push(job{st: l2i[b.Label], wait: labins[b.Label]})
				}

				path = append(path, df.ToPred(b.Expr, df.PredNotHeld))
				i++
			}

			tr.V("job_done").Printw("job done", "st", j.st, "end", i)
		}

		tr.V("job_paths,job_b_paths").Printw("job branch paths", "bpath", bpath)
		tr.V("job_paths,job_l_paths").Printw("job labels paths", "lpath", lpath)

		// rebuild

		renm := map[ir.Expr]ir.Expr{}

		ren := func(id ir.Expr) ir.Expr {
			for {
				y, ok := renm[id]
				if !ok {
					return id
				}

				id = y
			}
		}

		ren2 := func(l, r ir.Expr) (_, _ ir.Expr) {
			return ren(l), ren(r)
		}

		renPred := func(p []df.Pred) []df.Pred {
			r := make([]df.Pred, len(p))

			for i, p := range p {
				r[i] = df.ToPred(ren(p.Expr()), p.Held())
			}

			return r
		}

		code := make([]ir.Expr, 0, len(f.Code))

		for i, id := range f.Code {
			var y ir.Expr
			x := p.Exprs[id]

			switch x := x.(type) {
			case ir.Args, ir.Out, ir.Imm:
				y = id
			case ir.B, ir.BCond, ir.Label:
				continue
			case ir.Add:
				l, r := ren2(x.L, x.R)
				if l == x.L && r == x.R {
					y = id
					break
				}

				y = p.alloc(ir.Add{L: l, R: r}, p.EType[id])
			case ir.Sub:
				l, r := ren2(x.L, x.R)
				if l == x.L && r == x.R {
					y = id
					break
				}

				y = p.alloc(ir.Sub{L: l, R: r}, p.EType[id])
			case ir.Mod:
				l, r := ren2(x.L, x.R)
				if l == x.L && r == x.R {
					y = id
					break
				}

				y = p.alloc(ir.Mod{L: l, R: r}, p.EType[id])
			case ir.Cmp:
				l, r := ren2(x.L, x.R)
				if l == x.L && r == x.R {
					y = id
					break
				}

				y = p.alloc(ir.Cmp{L: l, R: r, Cond: x.Cond}, p.EType[id])
			case ir.Phi:
				if len(x) == 1 {
					y = p.alloc(df.Alias(ren(x[0].Expr)), p.EType[id])
					break
				}

				lab := func() ir.Label {
					j := i
					for j >= 0 {
						id := f.Code[j]
						x := p.Exprs[id]

						if l, ok := x.(ir.Label); ok {
							return l
						}

						j--
					}

					panic(i)
				}()

				pref := lpath[lab]

				pref = pref[:func() int {
					i := 0

					for i < len(pref) {
						for _, p := range x {
							path := bpath[p.B]

							if i == len(path) || path[i] != pref[i] {
								return i
							}
						}

						i++
					}

					return i
				}()]

				m := make(df.Merge, len(x))

				for j, p := range x {
					m[j] = df.MergeOpt{
						Expr: ren(p.Expr),
						Pred: renPred(bpath[p.B][len(pref):]),
					}
				}

				y = p.alloc(m, p.EType[id])
			default:
				panic(x)
			}

			if id != y {
				renm[id] = y
			}

			code = append(code, y)
		}

		for i, id := range code {
			x := p.Exprs[id]

			tr.Printw("code after", "i", i, "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "x", x)
		}

		for _, id := range f.Out {
			tr.Printw("func out", "id", ren(id), "was", id)
		}
	}

	return b, nil
}

func (js *jobs) Push(j job) {
	defer func(j job) {
		tlog.V("job_push").Printw("push job", "st", j.st, "wait", j.wait, "from", loc.Caller(2), "queue", js.Data)
	}(j)

	for i, j0 := range js.Data {
		if j0.st != j.st {
			continue
		}

		j0.wait--

		js.Heap.Data[i] = j0
		js.Heap.Fix(i)

		return
	}

	if j.wait != 0 {
		j.wait--
	}

	js.Heap.Push(j)
}

func jobsLess(d []job, i, j int) bool {
	if d[i].wait == 0 && d[j].wait == 0 {
		return d[i].st < d[j].st
	}

	return d[i].wait == 0
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

func dup[T any](s []T) []T {
	return append([]T{}, s...)
}

func (j job) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	b = e.AppendMap(b, 1)
	b = e.AppendInt(b, j.st)
	b = e.AppendInt(b, j.wait)

	return b
}
