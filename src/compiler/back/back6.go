package back

import (
	"container/heap"
	"context"
	"fmt"

	"github.com/slowlang/slow/src/compiler/ir"
	"github.com/slowlang/slow/src/compiler/set"
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
		jobs2
	}

	jobs2 struct {
		heap.Interface
		q []job
	}

	job struct {
		st   int
		path []branch

		wait int
	}

	branch int // index << 2 | notTaken << 1 | taken << 0

	flatmap []int
)

const (
	bTaken = 1 << iota
	bNotTaken

	bMask = 1<<iota - 1
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

	{ // merges
		labels := []int{}                    // label -> index
		merges := make(flatmap, len(f.Code)) // label index -> branches

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
		fwd := set.MakeBitmap(64)

		jobs.Push(job{st: 0, path: []branch{}})

		for jobs.Len() != 0 {
			j := jobs.Pop()
			i := j.st
			path := j.path

			tr.V("job_start").Printw("job start", "st", j.st, "path", j.path, "wait", j.wait, "more", jobs.jobs2.q)

			for i < len(f.Code) {
				id := f.Code[i]
				x := p.Exprs[id]

				if lab, ok := x.(ir.Label); ok && merges[i] > 0 {
					if fwd.IsSet(i) {
						break
					}

					fwd.Set(i)

					var split branch

					if l := len(path); l > 0 {
						split = path[l-1]
						path = path[:l-1]
					}

					tr.V("job_merge").Printw("merge point", "i", i, "id", id, "lab", lab, "split", split, "path", path, "wait", j.wait, "inputs", merges[i])

					i++
					continue
				}

				if b, ok := x.(ir.B); ok {
					next := labels[b.Label]
					jobs.Push(job{st: next, path: path, wait: merges[next]})
					break
				}

				b, ok := x.(ir.BCond)
				if !ok {
					i++
					continue
				}

				if next := labels[b.Label]; true {
					jobs.Push(job{
						st:   next,
						path: append(path[:len(path):len(path)], makeBranch(i, bTaken)),
						wait: merges[next],
					})
				}

				path = append(path, makeBranch(i, bNotTaken))
				i++

				continue
			}

			tr.V("job_done").Printw("job done", "st", j.st, "end", i, "path", path, "started", j.path)
		}
	}

	return b, nil
}

func (js jobs2) Len() int      { return len(js.q) }
func (js jobs2) Swap(i, j int) { js.q[i], js.q[j] = js.q[j], js.q[i] }
func (js jobs2) Less(i, j int) bool {
	if a, b := (js.q[i].wait == 0), (js.q[j].wait == 0); a && !b {
		return true
	} else if b && !a {
		return false
	}

	return js.q[i].st < js.q[j].st
}

func (js *jobs) Push(j job) {
	tlog.V("jobs_push").Printw("job pushed", "st", j.st, "path", j.path, "wait", j.wait, "from", loc.Caller(1))

	for i, j0 := range js.q {
		if j0.st != j.st {
			continue
		}

		p := 0

		for p < len(j.path) && p < len(j0.path) && j.path[p].Index() == j0.path[p].Index() {
			j0.path[p] |= (j0.path[p] | j.path[p]) & bMask
			p++
		}

		//	tlog.Printw("jobs merged", "st", j.st, "path", j0.path[:p], "j0.path", j0.path, "j.path", j.path)

		j0.path = j0.path[:p]
		j0.wait--

		js.q[i] = j0

		heap.Fix(js.jobs2, i)

		return
	}

	if j.wait != 0 {
		j.wait--
	}

	js.q = append(js.q, j)

	heap.Fix(js.jobs2, len(js.q)-1)
}

func (js *jobs) Pop() job {
	j := js.q[0]

	n := js.Len() - 1
	js.Swap(0, n)
	js.q = js.q[:n]

	heap.Fix(js.jobs2, 0)

	return j
}

func makeBranch(i, taken int) branch {
	return branch(i<<2 | taken)
}

func (b branch) Index() int {
	return int(b >> 2)
}

func (b branch) Taken() int {
	return int(b & 3)
}

func (j job) TlogAppend(b []byte) []byte {
	var e tlwire.Encoder

	b = e.AppendMap(b, 3)

	b = e.AppendKeyInt(b, "st", j.st)
	b = e.AppendKeyInt(b, "wait", j.wait)
	b = e.AppendKeyInt(b, "path", len(j.path))

	return b
}

func (b branch) TlogAppend(buf []byte) []byte {
	var e tlwire.Encoder

	var c byte

	switch b.Taken() {
	case bTaken:
		c = '>'
	case bNotTaken:
		c = 'v'
	case bTaken | bNotTaken:
		c = '&'
	case 0:
		c = '_'
	default:
		c = '?'
	}

	return e.AppendFormat(buf, "%d%c", b.Index(), c)
}
