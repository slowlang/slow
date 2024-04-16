package back

import (
	"container/heap"
	"context"
	"fmt"

	"github.com/slowlang/slow/src/compiler/ir"
	"github.com/slowlang/slow/src/compiler/set"
	"tlog.app/go/errors"
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
		jobs2
	}

	jobs2 struct {
		l []job

		merges []int
	}

	job struct {
		i    int
		path []branch

		reached int
	}

	branch struct {
		i     int
		taken int
	}

	flatmap []int
)

const (
	bTaken = 1 << iota
	bNotTaken
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
	tr, ctx := tlog.SpawnFromContextAndWrap(ctx, "compile func", "name", fn.Name)
	defer tr.Finish("err", &err)

	f := &funContext{Func: fn}

	if tr.If("dump_func_before") {
		for i, id := range f.Code {
			x := p.Exprs[id]

			tr.Printw("code before", "i", i, "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}
	}

	{
		labels := []int{}                    // label -> index
		merges := make(flatmap, len(f.Code)) // index -> branches
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

		fwd := set.MakeBitmap(64)
		jobs := []int{0}

		for len(jobs) != 0 {
			j := jobs[len(jobs)-1]
			jobs = jobs[:len(jobs)-1]

			i := j

			for i < len(f.Code) {
				id := f.Code[i]
				x := p.Exprs[id]

				if _, ok := x.(ir.Label); ok {
					if fwd.IsSet(i) {
						// loop
						break
					}

					fwd.Set(i)
				}

				if b, ok := x.(ir.B); ok {
					i = labels[b.Label]
				}

				b, ok := x.(ir.BCond)
				if !ok {
					i++
					continue
				}

				jobs = append(jobs, i+1)

				i = labels[b.Label]
			}
		}
	}

	if false {
		labels := []int{}                      // label -> index
		merges := make(flatmap, len(f.Code)+1) // index -> inputs

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

		var jobs jobs
		var visited set.Bits[ir.Label]

		jobs.merges = merges

		{ // forward
			jobs.Push(job{i: 0, path: []branch{{i: -1}}})

			for jobs.Len() != 0 {
				j := jobs.Pop()
				i := j.i

				tr.Printw("job start", "st", i, "path", j.path, "rest", jobs.l, "reached", j.reached, "of", merges[i])

				if j.reached > 1 {
					tr.Printw("block found", "st", i, "base", j.path[len(j.path)-1])
				}

				for i < len(f.Code) {
					id := f.Code[i]
					x := p.Exprs[id]

					if l, ok := x.(ir.Label); ok {
						visited.Set(l)
					}

					if b, ok := x.(ir.B); ok {
						jobs.Push(job{
							i:    labels[b.Label],
							path: j.path,
						})

						break
					}

					b, ok := x.(ir.BCond)
					if !ok {
						i++
						continue
					}

					jobs.Push(job{
						i: labels[b.Label],
						path: append(j.path[:len(j.path):len(j.path)], branch{
							i:     i,
							taken: bTaken,
						}),
					})

					jobs.Push(job{
						i: i + 1,
						path: append(j.path, branch{
							i:     i,
							taken: bNotTaken,
						}),
					})

					break
				}

				tr.Printw("job done", "st", j.i, "i", i)
			}
		}

		tr.Printw("visited", "visited", visited, "merges", merges)
	}

	return b, nil
}

func (js *jobs) Push(j job) {
	for i := 0; i < len(js.l); i++ {
		j0 := js.l[i]
		if j0.i != j.i {
			continue
		}

		tlog.Printw("merge jobs", "j0", j0, "j", j)

		p := 0

		for p < len(j0.path) && p < len(j.path) && j0.path[p].i == j.path[p].i {
			j0.path[p].taken |= j.path[p].taken
			p++

			if j0.path[p-1].taken == (bNotTaken | bTaken) {
				break
			}
		}

		j0.path = j0.path[:p]
		j0.reached++

		js.l[i] = j0

		heap.Fix(js.jobs2, i)

		return
	}

	j.reached = 1
	js.l = append(js.l, j)

	heap.Push(js.jobs2, j)
}

func (js *jobs) Pop() job {
	_ = heap.Pop(js.jobs2)

	l := len(js.l)
	j := js.l[l-1]
	js.l = js.l[:l-1]

	return j
}

func (js jobs) Len() int { return len(js.l) }

func (js jobs2) Len() int { return len(js.l) }
func (js jobs2) Less(i, j int) bool {
	ok := !(js.l[i].reached < js.merges[i]) && (js.l[j].reached < js.merges[j])
	if ok {
		return ok
	}

	return js.l[i].i < js.l[j].i
}
func (js jobs2) Swap(i, j int) { js.l[i], js.l[j] = js.l[j], js.l[i] }
func (js jobs2) Push(x any)    {}
func (js jobs2) Pop() any      { return nil }

func (j job) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	b = e.AppendMap(b, 2)

	b = e.AppendKeyInt(b, "st", j.i)

	b = e.AppendKey(b, "path")
	b = e.AppendValue(b, j.path)

	return b
}

func (br branch) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	b = e.AppendMap(b, 1)

	b = e.AppendInt(b, br.i)
	b = e.AppendInt(b, br.taken)

	return b
}

func (m flatmap) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	b = e.AppendMap(b, -1)

	for i, v := range m {
		if v == 0 {
			continue
		}

		b = e.AppendInt(b, i)
		b = e.AppendInt(b, v)
	}

	b = e.AppendBreak(b)

	return b
}
