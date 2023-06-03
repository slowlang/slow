package back

import (
	"context"
	"fmt"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
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
		//	e2b map[ir.Expr]int
		//	l2e map[ir.Label]ir.Expr
		//	lin map[ir.Label][]ir.Expr

		loop []int
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
		"state", f.State, "effect", f.Effect)
	defer tr.Finish("err", &err)

	p.funContext = &funContext{}

	if tr.If("dump_func_before") {
		tr.Printw("func before")

		for _, id := range f.Code {
			x := p.Exprs[id]

			tr.Printw("code", "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}
	}

	err = c.walkBlocks(ctx, p, f)
	if err != nil {
		return nil, errors.Wrap(err, "walk blocks")
	}

	return b, nil
}

func (c *Compiler) walkBlocks(ctx context.Context, p *pkgContext, f *ir.Func) (err error) {
	tr := tlog.SpanFromContext(ctx)

	i2b := make([]int, len(f.Code))

	l2i := make(map[ir.Label]int)
	lin := make(map[ir.Label][]int)

	bb := 0
	next := false

	for i, id := range f.Code {
		x := p.Exprs[id]

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

		path = path[:start.l]

		i := start.i

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
				//stack = append(stack, i+1)
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

		tr.Printw("loop", "nodes", visit)
	}

	if tr.If("dump_func_struct") {
		tr.Printw("func structure")

		for i, id := range f.Code {
			x := p.Exprs[id]

			args := []interface{}{"bb", i2b[i], "loop", loop[i], "i", i, "id", id, "tp", p.EType[id], "typ", tlog.NextAsType, x, "val", x}

			if l, ok := x.(ir.Label); ok {
				args = append(args, "l_in", lin[l])
			}

			tr.Printw("code", args...)
		}
	}

	return nil
}
