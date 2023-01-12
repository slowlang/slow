package back

import (
	"context"
	"fmt"
	"sort"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"

	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Arch interface {
		Alloc(id ir.Expr) int
		Free(int)
	}

	arch struct {
		freelist []int
	}

	Compiler struct{}

	/*
		state struct {
			f *ir.Func

			deps map[int][]int

			//	hint     map[ir.Expr]int
			compiled map[int]struct{}

			regnext int
			reg     map[ir.Expr]int

			//

			// codegen
			code  [][]any // block -> code
			ready map[ir.Expr]struct{}

			// color
			nextcolor int
			color     map[int]map[asm.Reg]int // block -> preg -> color
			colreg    map[int]int             // color -> reg
		}
	*/
)

func New() *Compiler { return nil }

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
	var q []int

	add := func(block int) {
		for _, x := range q {
			if x == block {
				return
			}
		}

		q = append(q, block)
	}

	allBlocks := func(fn func(block int, bp *ir.Block)) {
		q = q[:0]
		add(f.Entry)

		for i := 0; i < len(q); i++ {
			block := q[i]
			bp := &f.Blocks[block]

			for _, id := range bp.Code {
				x := f.Exprs[id]

				switch x := x.(type) {
				case ir.B:
					add(x.Block)
				case ir.BCond:
					add(x.Block)
				}
			}

			fn(block, bp)
		}
	}

	regmap := map[ir.Expr]int{}
	phifix := map[int]map[int][][2]int{} // block-to -> block-from -> list of {DST, SRC regs}

	printProg := func() { // print
		tlog.Printw("func", "name", f.Name, "entry", f.Entry, "in", len(f.In), "out", len(f.Out))

		for i, p := range f.In {
			x := f.Exprs[p.Expr]

			tlog.Printw("arg", "block", tlog.None, "id", p.Expr, "type", tlog.FormatNext("%T"), x, "val", x, "reg", i)
		}

		for _, p := range f.Out {
			x := f.Exprs[p.Expr]

			tlog.Printw("res", "block", tlog.None, "id", p.Expr, "type", tlog.FormatNext("%T"), x, "val", x, "reg", regmap[p.Expr])
		}

		allBlocks(func(block int, bp *ir.Block) {
			//	tlog.Printw("block", "block", block, "loop", b.Loop)

			for _, id := range bp.Phi {
				x := f.Exprs[id]

				args := []any{"block", block, "id", id, "type", tlog.FormatNext("%T"), x, "val", x}
				if reg, ok := regmap[id]; ok {
					args = append(args, "reg", reg)
				}
				//	if len(life[id]) != 0 {
				//		args = append(args, "life", life[id])
				//	}

				tlog.Printw("phi", args...)
			}

			for _, id := range bp.Code {
				x := f.Exprs[id]

				args := []any{"block", block, "id", id, "type", tlog.FormatNext("%T"), x, "val", x}
				if reg, ok := regmap[id]; ok {
					args = append(args, "reg", reg)
				}
				//	if len(life[id]) != 0 {
				//		args = append(args, "life", life[id])
				//	}

				tlog.Printw("code", args...)
			}
		})
	}

	printProg()

	{ // alloc
		// life times

		type Range [2]ir.Expr

		i2b := map[ir.Expr]int{}

		for _, p := range f.In {
			i2b[p.Expr] = f.Entry
		}

		allBlocks(func(block int, bp *ir.Block) {
			for _, id := range bp.Phi {
				i2b[id] = block
			}

			for _, id := range bp.Code {
				i2b[id] = block
			}
		})

		life := map[ir.Expr]ir.Expr{} // w -> r

		for _, id := range f.Blocks[f.Entry].Phi {
			x := f.Exprs[id].(ir.Phi)

			life[x[0]] = x[0]
		}

		br := func(brid ir.Expr, block, to int) {
			bp := &f.Blocks[to]

			for _, id := range bp.Phi {
				x := f.Exprs[id].(ir.Phi)

				for _, xx := range x {
					if i2b[xx] != block {
						continue
					}

					life[xx] = brid
				}
			}
		}

		allBlocks(func(block int, bp *ir.Block) {
			for _, id := range bp.Code {
				x := f.Exprs[id]

				switch x := x.(type) {
				case ir.Add:
					life[x.L] = id
					life[x.R] = id
				case ir.Sub:
					life[x.L] = id
					life[x.R] = id
				case ir.Mul:
					life[x.L] = id
					life[x.R] = id
				case ir.Cmp:
					life[x.L] = id
					life[x.R] = id
				case ir.B:
					br(id, block, x.Block)
				case ir.BCond:
					life[x.Expr] = id

					br(id, block, x.Block)
				}
			}
		})

		intersect := func(x, y ir.Expr) bool {
			if i2b[x] != i2b[y] {
				return false
			}

			xe, ok := life[x]
			if !ok {
				panic(x)
			}

			ye, ok := life[y]
			if !ok {
				panic(y)
			}

			return x < ye && y < xe
		}

		byw := make([]Range, 0, len(life))

		for w, r := range life {
			byw = append(byw, Range{w, r})
		}

		sort.Slice(byw, func(i, j int) bool { return byw[i][0] < byw[j][0] })

		for _, r := range byw {
			tlog.Printw("life", "block", i2b[r[0]], "w", r[0], "r", r[1])
		}

		// merge ranges

		group := map[ir.Expr]int{} // expr -> group
		content := map[int][]ir.Expr{}

		add := func(id ir.Expr) {
			gr := int(id)
			group[id] = gr
			content[gr] = []ir.Expr{id}
		}

		merge := func(x, y ir.Expr) {
			xgr := group[x]
			ygr := group[y]

			if xgr == ygr {
				return
			}

			for _, xc := range content[xgr] {
				for _, yc := range content[ygr] {
					if intersect(xc, yc) {
						return
					}
				}
			}

			for _, yc := range content[ygr] {
				group[yc] = xgr
			}

			content[xgr] = append(content[xgr], content[ygr]...)
			delete(content, ygr)
		}

		for _, p := range f.In {
			add(p.Expr)
		}

		allBlocks(func(block int, bp *ir.Block) {
			for _, id := range bp.Phi {
				add(id)
			}
			for _, id := range bp.Code {
				x := f.Exprs[id]

				switch x.(type) {
				case ir.Imm, ir.Add, ir.Sub, ir.Mul, ir.Cmp:
					add(id)
				case ir.B, ir.BCond:
				default:
					panic(x)
				}
			}
		})

		tlog.Printw("groups initial", "groups", len(content), "groups", content)

		allBlocks(func(block int, bp *ir.Block) {
			for _, id := range bp.Phi {
				x := f.Exprs[id].(ir.Phi)

				for _, xx := range x {
					merge(id, xx)
				}
			}
		})

		tlog.Printw("groups after phi", "groups", len(content), "groups", content)

		allBlocks(func(block int, bp *ir.Block) {
			for _, id := range bp.Code {
				x := f.Exprs[id]

				switch x := x.(type) {
				case ir.Add:
					merge(id, x.L)
					merge(id, x.R)
				case ir.Sub:
					merge(id, x.L)
					merge(id, x.R)
				case ir.Mul:
					merge(id, x.L)
					merge(id, x.R)
				case ir.Cmp:
				case ir.Imm, ir.B, ir.BCond:
				default:
					panic(x)
				}
			}
		})

		tlog.Printw("groups after ops", "groups", len(content), "groups", content)

		for i, p := range f.In {
			if i < len(f.Out) {
				merge(p.Expr, f.Out[i].Expr)
			}
		}

		for gra, la := range content {
			for grb, lb := range content {
				if gra == grb {
					continue
				}

				merge(la[0], lb[0])
			}
		}

		tlog.Printw("groups after all-with-all", "groups", len(content), "groups", content)

		tlog.Printw("group", "expr2group", group)

		// assign registers

		gr2reg := map[int]int{}

		for i, p := range f.In {
			regmap[p.Expr] = i
			gr2reg[group[p.Expr]] = i
		}

		for gr := range content {
			if _, ok := gr2reg[gr]; ok {
				continue
			}

			reg := len(gr2reg)
			gr2reg[gr] = reg
		}

		for id, gr := range group {
			regmap[id] = gr2reg[gr]
		}

		tlog.Printw("group", "gr2reg", gr2reg)

		// phi moves

		allBlocks(func(block int, bp *ir.Block) {
			for _, id := range bp.Phi {
				x := f.Exprs[id].(ir.Phi)

				for _, xx := range x {
					if group[id] == group[xx] {
						continue
					}

					from := i2b[xx]

					if phifix[block] == nil {
						phifix[block] = map[int][][2]int{}
					}

					phifix[block][from] = append(phifix[block][from], [2]int{gr2reg[group[id]], gr2reg[group[xx]]})
				}
			}
		})
	}

	reg := func(id ir.Expr) int {
		r, ok := regmap[id]
		if !ok {
			//	panic(id)
			return -1
		}

		return r
	}

	{ // compile

		b = fmt.Appendf(b, `
.global _%v
.align 4
_%[1]v:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP

`, f.Name)

		for i, p := range f.In {
			b = fmt.Appendf(b, "	// arg %d expr %d\n", i, p.Expr)
		}

		fixlabel := func(f, t int) string {
			fix := ""

			if l := phifix[f][t]; len(l) != 0 {
				fix = fmt.Sprintf("_phi_%d", f)
			}

			return fix
		}

		allBlocks(func(block int, bp *ir.Block) {
			for from, l := range phifix[block] {
				b = fmt.Appendf(b, "block_%v_phi_%v:\n", block, from)

				for _, r := range l {
					b = fmt.Appendf(b, "	MOV	X%d, X%d\n", r[0], r[1])
				}

				b = fmt.Appendf(b, "	B	block_%v\n", block)
			}

			b = fmt.Appendf(b, "block_%v:\n", block)

			for _, id := range bp.Code {
				x := f.Exprs[id]

				switch x := x.(type) {
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
					fix := fixlabel(x.Block, block)

					b = fmt.Appendf(b, "	B	block_%v%v\n", x.Block, fix)
				case ir.BCond:
					fix := fixlabel(x.Block, block)

					b = fmt.Appendf(b, "	B.%v	block_%v%v\n", cond2asm(x.Cond), x.Block, fix)
				default:
					panic(x)
				}
			}
		})

		for i, p := range f.Out {
			//	if regmap[p.Expr] != i {
			//		b = fmt.Appendf(b, "	MOV	X%d, X%d	// res %d  expr %d\n", i, regmap[p.Expr], i, p.Expr)
			//	} else {
			b = fmt.Appendf(b, "	// res %d  expr %d\n", i, p.Expr)
			//	}
		}

		b = fmt.Appendf(b, `
	LDP     FP, LR, [SP], #16
	RET
`)
	}

	return b, nil
}

func exprIn(e ir.Expr, in []ir.Expr) bool {
	for _, x := range in {
		if e == x {
			return true
		}
	}

	return false
}

func cond2asm(cond ir.Cond) string {
	switch cond {
	case "<":
		return "LT"
	default:
		panic(cond)
	}
}
