package back

import (
	"context"
	"fmt"

	"github.com/slowlang/slow/src/compiler/df"
	"github.com/slowlang/slow/src/compiler/ir"
	"github.com/slowlang/slow/src/compiler/set"
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

	jobs struct {
		heap.Heap[job]
	}

	job struct {
		st   int
		path []df.Pred

		wait int
	}

	path []df.Pred

	flatmap []int
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

	f := &funContext{Func: fn}

	if tr.If("dump_func_before") {
		for i, id := range f.Code {
			x := p.Exprs[id]

			tr.Printw("code before", "i", i, "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}
	}

	if true { // merges
		labels := []int{}                    // label -> index
		merges := make(flatmap, len(f.Code)) // label index -> branches
		bpaths := make(map[ir.Expr][]df.Pred)

		lastLabel := -1

		for i, id := range f.Code {
			x := p.Exprs[id]

			if l, ok := x.(ir.Label); ok {
				labels = sliceSet(labels, l, i)

				lastLabel = i
			}

			if p, ok := x.(ir.Phi); ok {
				merges[lastLabel] = len(p)
			}
		}

		jobs := jobs{Heap: heap.Heap[job]{Less: jobsLess}}
		fwd := set.MakeBitmap(64)

		jobs.Push(job{st: 0})

		for jobs.Len() != 0 {
			j := jobs.Pop()
			i := j.st
			path := j.path

			tr.V("job_start").Printw("job start", "st", j.st, "path", j.path, "wait", j.wait, "more", jobs.Data)

			for i < len(f.Code) {
				id := f.Code[i]
				x := p.Exprs[id]

				if lab, ok := x.(ir.Label); ok && merges[i] > 0 {
					if fwd.IsSet(i) {
						break
					}

					fwd.Set(i)

					var split df.Pred

					if l := len(path); l > 0 {
						split = path[l-1]
						path = dup(path[:l-1])
					}

					tr.V("job_merge").Printw("merge point", "i", i, "id", id, "lab", lab, "split", split, "path", path, "wait", j.wait, "inputs", merges[i])

					i++

					{
						id := f.Code[i]
						x := p.Exprs[id]

						p, ok := x.(ir.Phi)
						if !ok {
							continue
						}

						for _, br := range p {
							tr.V("job_merge").Printw("merge from", "i", "?", "id", br.B, "path", bpaths[br.B])
						}
					}

					continue
				}

				if b, ok := x.(ir.B); ok {
					next := labels[b.Label]
					jobs.Push(job{st: next, path: path, wait: merges[next]})

					tr.V("job_b").Printw("job branch", "i", i, "id", id, "to", next, "path", path, "base", j.path)
					bpaths[id] = dup(path)

					break
				}

				b, ok := x.(ir.BCond)
				if !ok {
					i++
					continue
				}

				if next := labels[b.Label]; true {
					path := append(path[:len(path):len(path)], df.ToPred(b.Expr, df.PredHeld))
					jobs.Push(job{
						st:   next,
						path: path,
						wait: merges[next],
					})

					tr.V("job_b").Printw("job branch", "to", next, "path", path, "base", j.path)
					bpaths[id] = dup(path)
				}

				path = append(path, df.ToPred(b.Expr, df.PredNotHeld))
				i++

				continue
			}

			tr.V("job_done").Printw("job done", "st", j.st, "end", i, "path", path, "started", j.path)
		}

		code := make([]ir.Expr, 0, len(f.Code))
		rename := make(map[ir.Expr]ir.Expr)
		ren := func(x ir.Expr) ir.Expr {
			for {
				y, ok := rename[x]
				if !ok {
					return x
				}

				x = y
			}
		}

		for i, id := range f.Code {
			x := p.Exprs[id]

			switch x := x.(type) {
			case ir.Args, ir.Imm, ir.Add, ir.Cmp:
				code = append(code, id)
			case ir.Label, ir.B, ir.BCond:
				// skip
			case ir.Phi:
				if len(x) == 1 {
					y := ren(x[0].Expr)
					z := p.alloc(df.Alias(y), p.EType[y])

					code = append(code, z)
					rename[id] = z

					continue
				}

				var m df.Merge

				for _, pb := range x {
					y := ren(pb.Expr)

					o := df.MergeOpt{
						Expr: y,
						Pred: bpaths[pb.B],
					}

					m = append(m, o)
				}

				tr.Printw("phi", "i", i, "id", id, "merge", m)

				z := p.alloc(m, p.EType[id])

				code = append(code, z)
				rename[id] = z
			default:
				panic(x)
			}
		}

		for i, id := range code {
			x := p.Exprs[id]

			tr.Printw("code df", "i", i, "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}
	}

	return b, nil
}

func jobsLess(d []job, i, j int) bool {
	if a, b := (d[i].wait == 0), (d[j].wait == 0); a && !b {
		return true
	} else if b && !a {
		return false
	}

	return d[i].st < d[j].st
}

func (js *jobs) Push(j job) {
	tlog.V("jobs_push").Printw("job pushed", "st", j.st, "path", j.path, "wait", j.wait, "from", loc.Caller(1))

	for i, j0 := range js.Data {
		if j0.st != j.st {
			continue
		}

		p := 0

		for p < len(j.path) && p < len(j0.path) && j.path[p] == j0.path[p] {
			p++
		}

		//	tlog.Printw("jobs merged", "st", j.st, "path", j0.path[:p], "j0.path", j0.path, "j.path", j.path)

		j0.path = j0.path[:p]
		j0.wait--

		js.Data[i] = j0

		js.Heap.Fix(i)

		return
	}

	if j.wait != 0 {
		j.wait--
	}

	js.Heap.Push(j)
}

func (j job) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	b = e.AppendMap(b, 3)

	b = e.AppendKeyInt(b, "st", j.st)
	b = e.AppendKeyInt(b, "wait", j.wait)
	b = e.AppendKeyInt(b, "path", len(j.path))

	return b
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
