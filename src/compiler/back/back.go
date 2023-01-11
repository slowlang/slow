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

	if false {
		i2b := map[ir.Expr]int{}
		life := map[ir.Expr][]ir.Expr{}
		phipar := map[ir.Expr]ir.Expr{}

		{ // lifetime
			buse := map[int][]ir.Expr{}
			bin := map[int][]int{}
			phi := map[ir.Expr]ir.Phi{}
			phib := map[ir.Expr]int{}

			for _, p := range f.In {
				i2b[p.Expr] = f.Entry
			}

			for _, p := range f.Out {
				life[p.Expr] = []ir.Expr{-1}
			}

			for block := range f.Blocks {
				b := &f.Blocks[block]

				for _, id := range b.Phi {
					x := f.Exprs[id].(ir.Phi)
					phi[id] = x
					phib[id] = block

					for _, x := range x {
						buse[block] = append(buse[block], x)
						phipar[x] = id
					}

					i2b[id] = block

					tlog.Printw("phi", "id", id, "val", x)
				}

				for _, id := range b.Code {
					i2b[id] = block
				}
			}

			br := func(id ir.Expr, from, to int) {
				for _, escape := range buse[to] {
					if i2b[escape] != from {
						continue
					}

					life[escape] = append(life[escape], id)
				}

				bin[to] = append(bin[to], from)
			}

			allBlocks(func(block int, bp *ir.Block) {
				for _, id := range bp.Code {
					x := f.Exprs[id]

					// TODO

					switch x := x.(type) {
					case ir.Add:
						life[x.L] = append(life[x.L], id)
						life[x.R] = append(life[x.R], id)
					case ir.Cmp:
						life[x.L] = append(life[x.L], id)
						life[x.R] = append(life[x.R], id)
					case ir.B:
						br(id, block, x.Block)
					case ir.BCond:
						life[x.Expr] = append(life[x.Expr], id)

						br(id, block, x.Block)
					}
				}
			})

			tlog.Printw("i2b", "i2b", i2b)
			tlog.Printw("phi", "phi", phi)
			tlog.Printw("phi", "phipar", phipar)
			tlog.Printw("bin", "bin", bin)
			tlog.Printw("buse", "buse", buse)

			for id, l := range life {
				tlog.Printw("life", "id", id, "uses", l)
			}
		}

		last := func(id ir.Expr) ir.Expr {
			l, ok := life[id]
			if !ok {
				panic(id)
			}

			return l[len(l)-1]
		}

		intersect := func(x, y ir.Expr) bool {
			if i2b[x] != i2b[y] {
				return false
			}

			return x < last(y) && y < last(x)
		}

		regmap := map[ir.Expr]int{}

		if false { // alloc
			isLastUse := func(block int, id, x ir.Expr) bool {
				l := life[x]

				return l[len(l)-1] == id
			}

			for i, p := range f.In {
				regmap[p.Expr] = i
			}

			allBlocks(func(block int, bp *ir.Block) {
				tlog.Printw("alloc block", "block", block, "regmap", regmap)

				a := newAArch64()

				for _, id := range bp.Phi {
					x := f.Exprs[id].(ir.Phi)

					reg := -1

					for _, x := range x {
						r, ok := regmap[x]
						if !ok {
							panic(id)
						}

						if reg == -1 {
							reg = r
						} else if reg != r {
							panic(fmt.Sprintf("phi %d:%v  reg %v and %v", id, f.Exprs[id], reg, r))
						}
					}

					regmap[id] = reg
					a.Use(id, reg)
				}

				for _, id := range bp.Code {
					x := f.Exprs[id]

					switch x := x.(type) {
					case ir.Imm:
						reg := a.Alloc(id)
						regmap[id] = reg
					case ir.Add:
						if isLastUse(block, id, x.R) {
							a.Free(regmap[x.R])
						}
						if isLastUse(block, id, x.L) {
							a.Free(regmap[x.L])
						}

						reg := a.Alloc(id)
						regmap[id] = reg
					case ir.Cmp:
						if isLastUse(block, id, x.R) {
							a.Free(regmap[x.R])
						}
						if isLastUse(block, id, x.L) {
							a.Free(regmap[x.L])
						}
					case ir.B:
					case ir.BCond:
					default:
						panic(x)
					}
				}
			})
		}

		{ // alloc
			_ = intersect

			for block := range f.Blocks {
				bp := &f.Blocks[block]

				_ = bp
			}
		}

		tlog.Printw("regmap", "regmap", regmap)
	}

	regmap := map[ir.Expr]int{}

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
			gr := len(group)
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
				case ir.Imm, ir.Add, ir.Cmp:
					add(id)
				case ir.B, ir.BCond:
				default:
					panic(x)
				}
			}
		})

		tlog.Printw("groups initial", "groups", len(content))

		allBlocks(func(block int, bp *ir.Block) {
			for _, id := range bp.Phi {
				x := f.Exprs[id].(ir.Phi)

				for _, xx := range x {
					merge(id, xx)
				}
			}
		})

		tlog.Printw("groups after phi", "groups", len(content))

		allBlocks(func(block int, bp *ir.Block) {
			for _, id := range bp.Code {
				x := f.Exprs[id]

				switch x := x.(type) {
				case ir.Add:
					merge(id, x.L)
					merge(id, x.R)
				case ir.Cmp:
				case ir.Imm, ir.B, ir.BCond:
				default:
					panic(x)
				}
			}
		})

		tlog.Printw("groups after ops", "groups", len(content))

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

		tlog.Printw("groups after all-with-all", "groups", len(content))

		tlog.Printw("group", "group", group, "content", content)

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
	}

	{ // print
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

		allBlocks(func(block int, bp *ir.Block) {
			b = fmt.Appendf(b, "block_%v:\n", block)

			for _, id := range bp.Code {
				x := f.Exprs[id]

				switch x := x.(type) {
				case ir.Imm:
					b = fmt.Appendf(b, "	MOV	X%d, #%d	// expr %d\n", reg(id), x, id)
				case ir.Add:
					b = fmt.Appendf(b, "	ADD	X%d, X%d, X%d	// expr %d\n", reg(id), reg(x.L), reg(x.R), id)
				case ir.Cmp:
					b = fmt.Appendf(b, "	CMP	X%d, X%d	// expr %d\n", reg(x.L), reg(x.R), id)
				case ir.B:
					b = fmt.Appendf(b, "	B	block_%v\n", x.Block)
				case ir.BCond:
					b = fmt.Appendf(b, "	B.%v	block_%v\n", cond2asm(x.Cond), x.Block)
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

/*
func (c *Compiler) compileFunc4(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	i2b := make([]int, len(f.Exprs))

	for i := range i2b {
		i2b[i] = -1
	}

	for block, b := range f.Blocks {
		for _, id := range b.Phi {
			i2b[id] = block
		}

		for _, id := range b.Code {
			i2b[id] = block
		}
	}

	tlog.Printw("i2b", "i2b", i2b)

	//

	type life struct {
		W int
		R int
	}

	type block struct {
		In       []ir.Expr
		Out      map[int][]ir.Expr // block -> []expr
		OutBlock map[ir.Expr]int   // expr -> block

		use map[ir.Expr]*life
	}

	bs := make([]block, len(f.Blocks))

	for block, b := range f.Blocks {
		bb := &bs[block]

		bb.Out = map[int][]ir.Expr{}
		bb.OutBlock = map[ir.Expr]int{}

		for _, id := range b.Code {
			if _, ok := f.Exprs[id].(ir.Arg); ok {
				bb.In = append(bb.In, id)
			}

			if x, ok := f.Exprs[id].(ir.B); ok {
				bb.OutBlock[id] = x.Block
			}

			if x, ok := f.Exprs[id].(ir.BCond); ok {
				bb.OutBlock[id] = x.Block
			}
		}
	}

	for block, b := range f.Blocks {
		bb := &bs[block]

		for _, id := range b.Phi {
			p := f.Exprs[id].(ir.Phi)

			bb.In = append(bb.In, id)

			for _, p := range p {
				bo := &bs[i2b[p]]
				bo.Out[block] = append(bo.Out[block], p)
			}
		}
	}

	//	for _, p := range f.Out {
	//		bs[0].Out[-1] = append(bs[0].Out[-1], p.Expr)
	//	}

	for block, b := range f.Blocks {
		bb := &bs[block]

		bb.use = map[ir.Expr]*life{}

		for _, e := range bb.In {
			bb.use[e] = &life{W: -1, R: -1}
		}

		for i, id := range b.Code {
			e := f.Exprs[id]

			switch e.(type) {
			case ir.Arg, ir.Imm,
				ir.Add, ir.Cmp:
				bb.use[id] = &life{W: i, R: -1}
			}

			switch e := e.(type) {
			case ir.Arg, ir.Imm:
			case ir.B:
			case ir.BCond:
				bb.use[e.Expr].R = i
			case ir.Add:
				bb.use[e.L].R = i
				bb.use[e.R].R = i
			case ir.Cmp:
				bb.use[e.L].R = i
				bb.use[e.R].R = i
			default:
				panic(e)
			}
		}
	}

	for block, bb := range bs {
		tlog.Printw("block", "block", block, "in", bb.In, "out", bb.Out, "out_block", bb.OutBlock)
	}

	for block, bb := range bs {
		tlog.Printw("block", "block", block, "use", bb.use)
	}

	X := map[ir.Expr]map[ir.Expr]struct{}{} // expr -> intersect

	for block, b := range f.Blocks {
		bb := &bs[block]
		_ = bb
		_ = b

		for i, id := range b.Code {
			if _, ok := bb.use[id]; !ok {
				continue
			}

			if X[id] == nil {
				X[id] = map[ir.Expr]struct{}{}
			}

			for x, life := range bb.use {
				if id == x {
					continue
				}

				//	tlog.Printw("xx", "id", id, "i", i, "x", x, "life", life)

				if i >= life.W && (i < life.R || life.R == -1) {
					if X[x] == nil {
						X[x] = map[ir.Expr]struct{}{}
					}

					X[id][x] = struct{}{}
					X[x][id] = struct{}{}
				}
			}
		}
	}

	tlog.Printw("intersections", "X", X)

	{ // print
		tlog.Printw("func", "name", f.Name, "in", f.In, "out", f.Out)

		//	for block := range f.Blocks {
		//		b := f.Blocks[block]
		//
		//		for _, p := range b.Phi {
		//			tlog.Printw("block", "id", "__", "block", block, "phi", p, "exprs", f.Exprs[p])
		//		}
		//	}

		//	for _, p := range f.In {
		//		e := f.Exprs[p.Expr]
		//		tlog.Printw("arg", "block", "_", "id", p.Expr, "type", tlog.FormatNext("%T"), e, "val", e)
		//	}

		printBlock := func(block int) {
			b := f.Blocks[block]
			bb := &bs[block]

			tlog.Printw("block", "block", block, "in", bb.In, "out", bb.Out, "out_block", bb.OutBlock)

			for _, id := range b.Phi {
				e := f.Exprs[id]

				var x tlog.RawMessage
				if list, ok := X[id]; ok {
					x = tlog.AppendKVs(nil, []interface{}{"x", list})
				}

				tlog.Printw("phi", "block", block, "id", id, "type", tlog.FormatNext("%T"), e, "val", e, x)
			}

			for _, id := range b.Code {
				e := f.Exprs[id]

				var x tlog.RawMessage
				if list, ok := X[id]; ok {
					x = tlog.AppendKVs(nil, []interface{}{"x", list})
				}

				tlog.Printw("expr", "block", block, "id", id, "type", tlog.FormatNext("%T"), e, "val", e, x)
			}
		}

		for block := 1; block < len(f.Blocks); block++ {
			printBlock(block)
		}

		printBlock(0)
	}

	//

	regmap := map[ir.Expr]int{}
	need := map[ir.Expr]int{}

	for i, p := range f.In {
		regmap[p.Expr] = i
	}

	for i, p := range f.Out {
		need[p.Expr] = i
	}

	allocBlock := func(block int) {
		b := &f.Blocks[block]

		for _, id := range b.Phi {
			p := f.Exprs[id].(ir.Phi)

			if reg, ok := regmap[id]; ok {
				for _, p := range p {
					need[p] = reg
				}
			}

			if reg, ok := need[id]; ok {
				for _, p := range p {
					need[p] = reg
				}
			}
		}

		for _, id := range b.Code {
			iner, ok := f.Exprs[id].(ir.Iner)
			if !ok {
				continue
			}

			in := iner.In()

			_ = in
		}
	}

	allocBlock(0)

	tlog.Printw("regmap", "map", regmap, "need", need)

	return b, nil
}

func (c *Compiler) compileFunc3(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	type block struct {
		In  []ir.Expr
		Out []ir.Expr

		Above []int
		Below []int
	}

	var blocks []*block
	var labels []int
	i2b := make([]int, len(f.Code))

	{
		b := -1

		for i, e := range f.Code {
			if e, ok := e.(ir.Label); ok {
				b = -1

				for int(e) >= len(labels) {
					labels = append(labels, -1)
				}

				labels[e] = len(blocks)
			}

			if b == -1 {
				b = len(blocks)

				blocks = append(blocks, &block{})
			}

			i2b[i] = b

			switch e.(type) {
			case ir.B, ir.BCond:
				b = -1
			}
		}

		//	blocks = append(blocks, &block{}) // exit block
	}

	appendLink := func(l []ir.Expr, e ir.Expr) []ir.Expr {
		for _, x := range l {
			if x == e {
				return l
			}
		}

		return append(l, e)
	}

	appendBlock := func(l []int, e int) []int {
		for _, x := range l {
			if x == e {
				return l
			}
		}

		return append(l, e)
	}

	linkBlocks := func(b1, b2 int, e ir.Expr) {
		if b1 != -1 {
			blocks[b1].Out = appendLink(blocks[b1].Out, e)
			blocks[b1].Below = appendBlock(blocks[b1].Below, b2)
		}

		if b2 != -1 {
			blocks[b2].In = appendLink(blocks[b2].In, e)
			blocks[b2].Above = appendBlock(blocks[b2].Above, b1)
		}
	}

	for i, e := range f.Code {
		b := i2b[i]

		switch e := e.(type) {
		case ir.Arg:
			linkBlocks(-1, b, ir.Expr(i))
		case ir.Word:
		case ir.Add:
			if db := i2b[e.L]; db != b {
				linkBlocks(db, b, e.L)
			}
			if db := i2b[e.R]; db != b {
				linkBlocks(db, b, e.R)
			}
		case ir.Cmp:
			if db := i2b[e.L]; db != b {
				linkBlocks(db, b, e.L)
			}
			if db := i2b[e.R]; db != b {
				linkBlocks(db, b, e.R)
			}
		case ir.BCond:
			if db := i2b[e.Expr]; db != b {
				linkBlocks(db, b, e.Expr)
			}
		case ir.Phi:
			for _, p := range e {
				linkBlocks(i2b[p], b, p)
			}
		case ir.B, ir.Label:
		default:
			panic(e)
		}
	}

	for _, p := range f.Out {
		linkBlocks(i2b[p.Expr], -1, p.Expr)
	}

	tlog.Printw("labels", "labels", labels)
	tlog.Printw("func", "name", f.Name, "in", f.In, "out", f.Out)

	for j, b := range blocks {
		tlog.Printw("block", "block", j, "in", b.In, "out", b.Out, "above", b.Above, "below", b.Below)
	}

	for i, e := range f.Code {
		tlog.Printw("expr", "id", i, "block", i2b[i], "type", tlog.FormatNext("%T"), e, "val", e)
	}

	return b, nil
}

func (c *Compiler) compileFunc2(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	type block struct {
	}

	var blocks []*block
	var curBlock int = -1

	i2block := make([]int, len(f.Code))
	labels := []int{}
	jumps := map[int]int{} // i -> block

	for i, e := range f.Code {
		switch e := e.(type) {
		case ir.Label:
			curBlock = -1

			for int(e) >= len(labels) {
				labels = append(labels, -1)
			}

			labels[e] = len(blocks)
		}

		if curBlock == -1 {
			curBlock = len(blocks)
			blocks = append(blocks, &block{})
		}

		i2block[i] = curBlock

		switch e.(type) {
		case ir.B, ir.BCond:
			curBlock = -1
		}
	}

	blocksin := make([][]int, len(blocks)+1)

	lastB := false

	for i, e := range f.Code {
		if i > 0 && !lastB && i2block[i-1] != i2block[i] {
			blocksin[i2block[i]] = append(blocksin[i2block[i]], i2block[i-1])
		}

		var l ir.Label

		lastB = false

		switch e := e.(type) {
		case ir.B:
			l = e.Label

			lastB = true
		case ir.BCond:
			l = e.Label
		default:
			continue
		}

		if l == -1 {
			jumps[i] = len(blocks)
		} else {
			jumps[i] = labels[l]
		}

		blocksin[jumps[i]] = append(blocksin[jumps[i]], i2block[i])
	}

	phi := map[ir.Expr][]ir.Expr{}
	//	phi := [][2]ir.Expr{}
	live := make([][]int, len(f.Code)) // expr -> reads

	rephi := func(e ir.Expr) ir.Expr {
		for {
			ee := phi[e]
			if len(ee) == 0 {
				return e
			}

			e = ee[len(ee)-1]
		}
	}

	for i, e := range f.Code {
		switch e := e.(type) {
		case ir.Arg, ir.Word:
		case ir.B, ir.Label:
		case ir.BCond:
			live[e.Expr] = append(live[e.Expr], i)
		case ir.Add:
			live[e.L] = append(live[e.L], i)
			live[e.R] = append(live[e.R], i)
		case ir.Cmp:
			live[e.L] = append(live[e.L], i)
			live[e.R] = append(live[e.R], i)
		case ir.Phi:
			for _, e := range e {
				phi[e] = append(phi[e], ir.Expr(i))
				live[e] = append(live[e], i)
			}
		default:
			panic(e)
		}
	}

	for _, p := range f.Out {
		live[p.Expr] = append(live[p.Expr], len(f.Code))
	}

	tlog.Printw("info", "i2block", i2block)
	tlog.Printw("info", "blocks", blocks)
	tlog.Printw("info", "labels", labels, "jumps", jumps, "b_in", blocksin)
	tlog.Printw("info", "phi", phi)
	tlog.Printw("info", "live", live)

	tlog.Printw("func", "name", f.Name, "in", f.In, "out", f.Out)

	for id, e := range f.Code {
		tlog.Printw("expr", "id", id, "block", i2block[id], "type", tlog.FormatNext("%T"), e, "val", e)
	}

	regmap := make([]int, len(f.Code))

	for i := range regmap {
		regmap[i] = -1
	}

	freelist := []int{}
	nextreg := 0

	alloc := func(e ir.Expr) (r int) {
		ee := rephi(e)
		//	for s, ok := phi[e]; ok; s, ok = phi[e] {
		//		e = s
		//	}

		if r = regmap[ee]; r != -1 {
			return r
		}

		defer func() {
			tlog.Printw("alloc", "id", e, "e", e, "ee", ee, "reg", r, "freelist", freelist)
		}()

		if len(freelist) == 0 {
			r = nextreg
			nextreg++

			for _, e := range phi[e] {
				regmap[e] = r
			}

			regmap[ee] = r

			return r
		}

		l := len(freelist) - 1
		r = freelist[l]
		freelist = freelist[:l]

		for _, e := range phi[e] {
			regmap[e] = r
		}

		regmap[ee] = r

		return r
	}

	free := func(i int, e ir.Expr) {
		for _, u := range live[e] {
			if u > i {
				return
			}
		}

		reg := regmap[e]

		tlog.Printw("free", "id", i, "e", e, "ee", e, "reg", reg, "freelist", freelist)

		freelist = append(freelist, reg)
	}

	getreg := func(e ir.Expr) (r int) {
		ee := rephi(e)
		//	for s, ok := phi[e]; ok; s, ok = phi[e] {
		//		e = s
		//	}

		r = regmap[ee]
		if r == -1 {
			panic(fmt.Sprintf("no reg for %d -> %d", e, ee))
		}

		return r
	}

	for _, p := range f.In {
		regmap[p.Expr] = alloc(p.Expr)
	}

	for id, e := range f.Code {
		switch e := e.(type) {
		case ir.Arg:
		case ir.Word:
			alloc(ir.Expr(id))
		case ir.Add:
			free(id, e.R)
			free(id, e.L)

			alloc(ir.Expr(id))
		case ir.Cmp:
			free(id, e.R)
			free(id, e.L)

			alloc(ir.Expr(id))
		case ir.BCond:
			free(id, e.Expr)
		case ir.B, ir.Label:
		case ir.Phi:
		default:
			panic(e)
		}
	}

	tlog.Printw("reg map", "map", regmap)

	b = fmt.Appendf(b, `.global %s
.align 4
%[1]s:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP

`, f.Name)

	for id, e := range f.Code {
		switch e := e.(type) {
		case ir.Arg:
			reg := getreg(ir.Expr(id))

			b = fmt.Appendf(b, "	// Arg	X%d, %d	// expr %d\n", reg, e, id)
		case ir.Word:
			reg := getreg(ir.Expr(id))

			b = fmt.Appendf(b, "	MOV	X%d, #%d	// expr %d\n", reg, e, id)
		case ir.Add:
			l := getreg(e.L)
			r := getreg(e.R)
			reg := getreg(ir.Expr(id))

			b = fmt.Appendf(b, "	ADD	X%d, X%d, X%d	// expr %d\n", reg, l, r, id)
		case ir.Cmp:
			l := getreg(e.L)
			r := getreg(e.R)
			//	reg := getreg(ir.Expr(id))

			b = fmt.Appendf(b, "	CMP	X%d, X%d	// expr %d\n", l, r, id)
		case ir.BCond:
			//x := getreg(e.Expr)

			var cond string

			switch e.Cond {
			case "<":
				cond = "LT"
			case ">":
				cond = "GT"
			default:
				panic(e.Cond)
			}

			b = fmt.Appendf(b, "	B.%s	block_%v\n", cond, labels[e.Label])
		case ir.B:
			if e.Label == -1 {
				b = fmt.Appendf(b, "	B	ret_%s	// expr %d\n", f.Name, id)
				break
			}

			b = fmt.Appendf(b, "	B	block_%v\n", labels[e.Label])
		case ir.Label:
			b = fmt.Appendf(b, "block_%v:\n", i2block[id])
		case ir.Phi:
			b = fmt.Appendf(b, "	// phi X%d = exprs %v	// expr %d\n", getreg(ir.Expr(id)), e, id)
		default:
			panic(e)
		}
	}

	b = fmt.Appendf(b, `
ret_%s:
`, f.Name)

	for i, p := range f.Out {
		if i != regmap[p.Expr] {
			b = fmt.Appendf(b, "	MOV	X%d, #%d	// result\n", i, regmap[p.Expr])
		} else {
			b = fmt.Appendf(b, "	// result[%d] = X%d\n", i, regmap[p.Expr])
		}
	}

	b = fmt.Appendf(b, `
	LDP     FP, LR, [SP], #16
	RET
`)

	return b, nil
}

func (c *Compiler) compileFunc1(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	var q []ir.Expr
	s := map[ir.Expr]struct{}{}

	var calc func(id ir.Expr)

	calc = func(id ir.Expr) {
		if _, ok := s[id]; ok {
			return
		}

		switch x := f.Code[id].(type) {
		case ir.Phi:
			for _, e := range x {
				calc(e)
			}
		case ir.Add:
			calc(x.L)
			calc(x.R)
		case ir.Cmp:
			calc(x.L)
			calc(x.R)
		case ir.Arg:
		case ir.Word:
		case ir.B:
		case ir.BCond:
			calc(x.Expr)
		default:
			panic(x)
		}

		q = append(q, id)
		s[id] = struct{}{}
	}

	for _, p := range f.Out {
		calc(p.Expr)
	}

	tlog.Printw("calc queue", "queue", q)

	return b, nil
}

func (c *Compiler) compileFunc0(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	b = fmt.Appendf(b, `.global %s
.align 4
%[1]s:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP
`, f.Name)

	b = fmt.Appendf(b, `
ret_%s:
	LDP     FP, LR, [SP], #16
	RET
`, f.Name)

	return b, nil
}

func (c *Compiler) compileFunc(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	s := &state{
		f:    f,
		deps: map[int][]int{},
	}

	err = c.codegen(ctx, s, f)
	if err != nil {
		return nil, err
	}

	for block, l := range s.code {
		for i, x := range l {
			tlog.Printw("code", "block", block, "i", i, "x", tlog.Format{Fmt: "%T%[1]v", Args: []any{x}})
		}
	}

	err = c.color(ctx, s, f)
	if err != nil {
		return nil, err
	}

	return nil, nil

	b, err = c.compileFunc1(ctx, b, s, f)
	if err != nil {
		return nil, err
	}

	return b, nil
}

func (c *Compiler) compileFunc0(ctx context.Context, a Arch, b []byte, f *ir.Func) (_ []byte, err error) {
	b = fmt.Appendf(b, `.global %s
.align 4
%[1]s:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP
`, f.Name)

	s := &state{
		f:        f,
		deps:     map[int][]int{},
		compiled: map[int]struct{}{},
		regnext:  len(f.In),
		reg:      map[ir.Expr]int{},
	}

	for id := range f.Blocks {
		c.calcDeps(ctx, a, s, id)
	}

	tlog.Printw("block deps", "block_deps", s.deps)

	b, err = c.compileBlock0(ctx, a, b, s, 0)
	if err != nil {
		return nil, errors.Wrap(err, "")
	}

	b = fmt.Appendf(b, `
ret_%s:
	LDP     FP, LR, [SP], #16
	RET
`, f.Name)

	return b, nil
}

func (c *Compiler) codegen(ctx context.Context, s *state, f *ir.Func) (err error) {
	s.code = make([][]any, len(f.Blocks))
	//	s.phi = map[ir.Expr]ir.Phi{}

	for block := range f.Blocks {
		s.ready = map[ir.Expr]struct{}{}

		err = c.codegenBlock(ctx, s, f, block)
		if err != nil {
			return
		}
	}

	return
}

func (c *Compiler) codegenBlock(ctx context.Context, s *state, f *ir.Func, block int) (err error) {
	b := &f.Blocks[block]

	for _, e := range b.Out {
		s.code[block], err = c.codegenExpr(ctx, s, f, block, e, s.code[block])
		if err != nil {
			return
		}
	}

	for _, e := range b.Code {
		s.code[block], err = c.codegenExpr(ctx, s, f, block, e, s.code[block])
		if err != nil {
			return
		}
	}

	return
}

func (c *Compiler) codegenExpr(ctx context.Context, s *state, f *ir.Func, block int, e ir.Expr, l []any) (_ []any, err error) {
	b := &f.Blocks[block]

	//	tlog.Printw("codegen expr", "block", block, "expr", e, "expr_type", tlog.FormatNext("%T"), f.Exprs[e])

	if _, ok := f.Exprs[e].(ir.Arg); !ok && exprIn(e, b.In) {
		return l, nil
	}
	//	if exprIn(e, b.In) {
	//		return l, nil
	//	}

	if _, ok := s.ready[e]; ok {
		return l, nil
	}

	s.ready[e] = struct{}{}

	switch x := f.Exprs[e].(type) {
	case ir.Word:
		l = append(l, asm.Imm{
			Out:  [1]asm.Reg{asm.Reg(e)},
			Word: uint64(x),
		})
	case ir.Arg:
		l = append(l, asm.Arg{
			Out: [1]asm.Reg{asm.Reg(e)},
			Arg: int(x),
		})
	case ir.Phi:
		//	s.phi[e] = x
		l = append(l, asm.Phi{
			Out: [1]asm.Reg{asm.Reg(e)},
			In:  phi2regs(x),
		})
	case ir.Add:
		l, err = c.codegenExpr(ctx, s, f, block, x.Left, l)
		if err != nil {
			return nil, err
		}

		l, err = c.codegenExpr(ctx, s, f, block, x.Right, l)
		if err != nil {
			return nil, err
		}

		l = append(l, asm.Add{
			Out: [1]asm.Reg{asm.Reg(e)},
			In:  [2]asm.Reg{asm.Reg(x.Left), asm.Reg(x.Right)},
		})
	case ir.Cmp:
		l, err = c.codegenExpr(ctx, s, f, block, x.Left, l)
		if err != nil {
			return nil, err
		}

		l, err = c.codegenExpr(ctx, s, f, block, x.Right, l)
		if err != nil {
			return nil, err
		}

		l = append(l, asm.Cmp{
			Out: [1]asm.Reg{asm.Reg(e)},
			In:  [2]asm.Reg{asm.Reg(x.Left), asm.Reg(x.Right)},
		})
	case ir.BranchIf:
		l, err = c.codegenExpr(ctx, s, f, block, x.Expr, l)
		if err != nil {
			return nil, err
		}

		var cond asm.Cond

		switch x.Cond {
		case ">":
			cond = "GT"
		case "<":
			cond = "LT"
		default:
			return nil, errors.New("unsupported cond: %v", x.Cond)
		}

		l = append(l, asm.BCond{
			Cond:  cond,
			Label: asm.Label(x.Block),
			In:    [1]asm.Reg{asm.Reg(x.Expr)},
		})
	case ir.Branch:
		l = append(l, asm.B{
			Label: asm.Label(x.Block),
		})
	default:
		return nil, errors.New("unsupported expr: %T", x)
	}

	return l, nil
}

func (c *Compiler) color(ctx context.Context, s *state, f *ir.Func) (err error) {
	phi := map[asm.Reg]asm.Reg{} // renames

	for _, l := range s.code {
		for _, x := range l {
			p, ok := x.(asm.Phi)
			if !ok {
				continue
			}

			for _, r := range p.In {
				phi[r] = p.Out[0]
			}
		}
	}

	tlog.Printw("renames", "phi", phi)

	x := map[asm.Reg][]asm.Reg{}

	group := func(l []ir.Expr) {
		for i, e := range l {
			ee := asm.Reg(e)

			x[ee] = append(x[ee])

		inner:
			for j, e2 := range l {
				ee2 := asm.Reg(e2)

				for q, ok := phi[ee2]; ok; q, ok = phi[ee2] {
					ee2 = q
				}

				if ee == ee2 {
					continue
				}

				for _, h := range x[ee] {
					if h == asm.Reg(e2) {
						continue inner
					}
				}

				x[ee] = append(x[ee], asm.Reg(e2))
			}
		}
	}

	for _, b := range f.Blocks {
		tlog.Printw("block", "b", b)
		group(b.In)
		group(b.Out)
	}

	for reg, list := range x {
		tlog.Printw("intersections", "reg", reg, "list", list)
	}

	return
}

func (c *Compiler) color0(ctx context.Context, s *state, f *ir.Func) (err error) {
	s.color = map[int]map[asm.Reg]int{}

	for block := range f.Blocks {
		err = c.colorBlock(ctx, s, f, block)
		if err != nil {
			return
		}
	}

	colors := map[asm.Reg][]int{}
	backcol := map[int][]asm.Reg{}

	for block := range f.Blocks {
		for pr, col := range s.color[block] {
			colors[pr] = append(colors[pr], col)
			backcol[col] = append(backcol[col], pr)
		}
	}

	tlog.Printw("merged colors", "reg_color", colors, "back", backcol)

	fwd, bwd, first, last := c.calcPaths(s, f)

	tlog.Printw("block links", "first", first, "last", last, "fwd", fwd, "bwd", bwd)

	colreg := map[int]int{}
	regcol := map[int][]int{}

	regnext := 0

	alloc := func(col int) {
		defer func() {
			r := colreg[col]
			regcol[r] = append(regcol[r], col)
		}()

		colreg[col] = regnext
		regnext++

		tlog.Printw("alloc", "color", col, "reg", colreg[col], "from", loc.Caller(1))
	}

	allocArgs := func(block int) {
		for _, e := range f.Blocks[block].In {
			col := s.color[block][asm.Reg(e)]

			alloc(col)
		}
	}

	allocBlock := func(block int) {
		for _, col := range s.color[block] {
			if _, ok := colreg[col]; ok {
				continue
			}

			alloc(col)
		}
	}

	stitch := func(a, b int) {
		for _, e := range f.Blocks[b].In {
			col := s.color[b][asm.Reg(e)]
			pcol := s.color[a][asm.Reg(e)]

			reg := colreg[pcol]
			colreg[col] = reg
			regcol[reg] = append(regcol[reg], col)

			tlog.Printw("stitch", "block", b, "prev", a, "color", col, "prev_color", pcol, "reg", reg, "expr", e)
		}
	}

	allocArgs(first)

	q := []int{first}

	for i := 0; i < len(q); i++ {
		block := q[i]

		allocBlock(block)

		for _, next := range fwd[block] {
			stitch(block, next)

			add := true

			for _, b := range q {
				if b == next {
					add = false
				}
			}

			if add {
				q = append(q, next)
			}
		}
	}

	tlog.Printw("queue", "q", q)

	tlog.Printw("registers", "col_reg", colreg, "reg_col", regcol)

	s.colreg = colreg

	return
}

func (c *Compiler) colorBlock(ctx context.Context, s *state, f *ir.Func, block int) (err error) {
	type usage struct {
		W int
		R []int
	}

	u := map[asm.Reg]*usage{}
	phi := map[asm.Reg]asm.Reg{} // remap

	for _, e := range f.Blocks[block].In {
		u[asm.Reg(e)] = &usage{W: -1}
	}

	for i, x := range s.code[block] {
		switch x := x.(type) {
		case asm.Arg:
			u[x.Out[0]] = &usage{W: i}
		case asm.Imm:
			u[x.Out[0]] = &usage{W: i}
		case asm.Add:
			u[x.Out[0]] = &usage{W: i}

			for _, r := range x.In {
				u[r].R = append(u[r].R, i)
			}
		case asm.Cmp:
			u[x.Out[0]] = &usage{W: i}

			for _, r := range x.In {
				u[r].R = append(u[r].R, i)
			}
		case asm.BCond:
			for _, r := range x.In {
				u[r].R = append(u[r].R, i)
			}
		case asm.B:
		case asm.Phi:
			u[x.Out[0]] = &usage{W: i}

			for _, r := range x.In {
				u[r].R = append(u[r].R, i)

				phi[r] = x.Out[0]
			}
		default:
			panic(x)
		}

		//	tlog.Printw("color block", "block", block, "instr", i, "u", u)
	}

	for _, e := range f.Blocks[block].Out {
		r := asm.Reg(e)
		u[r].R = append(u[r].R, -1)
	}

	for r, u := range u {
		tlog.Printw("regs usage", "block", block, "preg", r, "usage", u)
	}

	reg := map[asm.Reg]int{}
	freelist := []int{}

	add := func(r asm.Reg) {
		for q, ok := phi[r]; ok; q, ok = phi[r] {
			defer func(orig asm.Reg) {
				reg[orig] = reg[r]
			}(r)

			r = q
		}

		if _, ok := reg[r]; ok {
			return
		}

		if l := len(freelist); l != 0 {
			reg[r] = freelist[l-1]
			freelist = freelist[:l-1]
			return
		}

		reg[r] = s.nextcolor
		s.nextcolor++
	}

	free := func(r asm.Reg) {
		if _, ok := phi[r]; ok {
			return
		}

		freelist = append(freelist, reg[r])
	}

	{
		for r, u := range u {
			if u.W == -1 {
				add(r)
				tlog.V("alloc").Printw("alloc reg", "block", block, "preg", r, "reg", reg[r])
			}
		}

		for i := range s.code {
			for r, u := range u {
				if u.R[len(u.R)-1] == i {
					free(r)
					tlog.V("alloc").Printw("free reg", "block", block, "preg", r, "reg", reg[r], "freelist", freelist)
				}
			}

			for r, u := range u {
				if u.W == i {
					add(r)
					tlog.V("alloc").Printw("alloc reg", "block", block, "preg", r, "reg", reg[r], "freelist", freelist)
				}
			}
		}
	}

	tlog.Printw("reg map", "block", block, "regmap", reg, "nextcolor", s.nextcolor, "phi", phi)
	tlog.Printw("block in-out", "block", block, "in", f.Blocks[block].In, "out", f.Blocks[block].Out)

	s.color[block] = reg

	return
}

func (c *Compiler) calcPaths(s *state, f *ir.Func) (fwd, bwd [][]int, first, last int) {
	fwd = make([][]int, len(f.Blocks))
	bwd = make([][]int, len(f.Blocks))

	link := func(a, b int) {
		fwd[a] = append(fwd[a], b)
		bwd[b] = append(bwd[b], a)
	}

	for block, b := range f.Blocks {
		for _, e := range b.Code {
			switch op := s.f.Exprs[e].(type) {
			case ir.Branch:
				link(block, op.Block)
			case ir.BranchIf:
				link(block, op.Block)
			}
		}
	}

	for block, l := range fwd {
		if len(l) == 0 {
			last = block
		}
		if len(bwd[block]) == 0 {
			first = block
		}
	}

	return
}

func (c *Compiler) calcDeps(ctx context.Context, a Arch, s *state, block int) {
	bp := &s.f.Blocks[block]

	for _, e := range bp.Code {
		switch op := s.f.Exprs[e].(type) {
		case ir.Branch:
			s.addDep(op.Block, block)
		case ir.BranchIf:
			s.addDep(op.Block, block)
		}
	}
}

func (c *Compiler) compileFunc1(ctx context.Context, b []byte, s *state, f *ir.Func) (_ []byte, err error) {
	b = fmt.Appendf(b, `.global %s
.align 4
%[1]s:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP
`, f.Name)

	fwd, _, first, last := c.calcPaths(s, f)

	q := []int{first}

	for i := 0; i < len(q); i++ {
		block := q[i]

		for _, next := range fwd[block] {
			add := true

			for _, b := range q {
				if b == next {
					add = false
				}
			}

			if add {
				q = append(q, next)
			}
		}

		b, err = c.compileBlock1(ctx, b, s, block)
		if err != nil {
			return
		}
	}

	for i, e := range f.Blocks[last].Out {
		col := s.color[last][asm.Reg(e)]
		reg, ok := s.colreg[col]
		if !ok {
			panic(e)
		}

		if reg == i {
			continue
		}

		b = fmt.Appendf(b, "	MOV	X%d, X%d\n", i, reg)
	}

	b = fmt.Appendf(b, `
	LDP     FP, LR, [SP], #16
	RET
`)

	return b, nil
}

func (c *Compiler) compileBlock1(ctx context.Context, b []byte, s *state, block int) (_ []byte, err error) {
	reg := func(pr asm.Reg) int {
		col, ok := s.color[block][pr]
		if !ok {
			panic(pr)
		}

		r, ok := s.colreg[col]
		if !ok {
			panic(pr)
		}

		return r
	}

	b = fmt.Appendf(b, "\nblock_%d:\n", block)

	for _, x := range s.code[block] {
		switch x := x.(type) {
		case asm.Arg:
			b = fmt.Appendf(b, "	// arg %d\n", x.Arg)
		case asm.Phi:
		case asm.Imm:
			b = fmt.Appendf(b, "	MOV	X%d, #%d\n", reg(x.Out[0]), x.Word)
		case asm.Add:
			b = fmt.Appendf(b, "	ADD	X%d, X%d, X%d\n", reg(x.Out[0]), reg(x.In[0]), reg(x.In[1]))
		case asm.Cmp:
			b = fmt.Appendf(b, "	CMP	X%d, X%d	// out reg %d\n", reg(x.In[0]), reg(x.In[1]), reg(x.Out[0]))
		case asm.BCond:
			b = fmt.Appendf(b, "	B.%s	block_%d	// in reg %d\n", x.Cond, x.Label, reg(x.In[0]))
		case asm.B:
			b = fmt.Appendf(b, "	B	block_%d\n", x.Label)
		default:
			return nil, errors.New("unsupported expr: %T", x)
		}
	}

	return b, nil
}

func (c *Compiler) compileBlock0(ctx context.Context, a Arch, b []byte, s *state, block int) (_ []byte, err error) {
	if _, ok := s.compiled[block]; ok {
		return b, nil
	}

	s.compiled[block] = struct{}{}

	buf, err := c.compileBlockData(ctx, a, nil, s, block)
	if err != nil {
		return nil, errors.Wrap(err, "")
	}

	for _, d := range s.deps[block] {
		b, err = c.compileBlock0(ctx, a, b, s, d)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}
	}

	b = append(b, buf...)

	return b, nil
}

func (c *Compiler) compileBlockData(ctx context.Context, a Arch, b []byte, s *state, block int) (_ []byte, err error) {
	bp := &s.f.Blocks[block]

	b = fmt.Appendf(b, "\nblock_%d:\n", block)

	for _, e := range bp.Out {
		_, b, err = c.compileExpr0(ctx, a, b, s, e, block)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}
	}

	for _, e := range bp.Code {
		switch x := s.f.Exprs[e].(type) {
		case ir.BranchIf:
			_, b, err = c.compileExpr0(ctx, a, b, s, x.Expr, block)
			if err != nil {
				return
			}

			cond := ""

			switch x.Cond {
			case "<":
				cond = "LS"
			case ">":
				cond = "GT"
			default:
				return nil, errors.New("unsupported operation: %q", x.Cond)
			}

			b = fmt.Appendf(b, "	B.%s	block_%d\n", cond, x.Block)
		case ir.Branch:
			b = fmt.Appendf(b, "	B	block_%d\n", x.Block)
		default:
			return nil, errors.New("unsupported op: %T", x)
		}
	}

	return b, nil
}

func (c *Compiler) compileExpr0(ctx context.Context, a Arch, b []byte, s *state, e ir.Expr, block int) (reg int, _ []byte, err error) {
	//	b = fmt.Appendf(b, "	// X%d <- expr %d  %T%[3]v\n", reg, e, f.Exprs[e])

	bp := &s.f.Blocks[block]
	if exprIn(e, bp.In) {
		return -1, b, nil
	}

	reg = s.alloc(e)

	tlog.Printw("alloc reg", "reg", reg, "expr", e, "block", block)

	switch x := s.f.Exprs[e].(type) {
	case ir.Word:
		b = fmt.Appendf(b, "	MOV	X%d, #%d	// expr %d\n", reg, x, e)
	case ir.Arg:
		if int(x) != reg {
			b = fmt.Appendf(b, "	MOV	X%d, X%d	// arg %d\n", reg, x, x)
		} else {
			b = fmt.Appendf(b, "	// arg %d is already in place\n", x)
		}
	case ir.Phi:
		b = fmt.Appendf(b, "	// X%d is %v	// expr %d\n", reg, x, e)
	//	for _, e := range x {
	//		s.hint[e] = reg
	//	}
	case ir.Cmp:
		var lr, rr int

		lr, b, err = c.compileExpr0(ctx, a, b, s, x.Left, block) //, reg+1)
		if err != nil {
			return
		}

		rr, b, err = c.compileExpr0(ctx, a, b, s, x.Right, block) //, reg+2)
		if err != nil {
			return
		}

		b = fmt.Appendf(b, "	CMP	X%d, X%d\n", lr, rr)
	case ir.Add:
		var lr, rr int

		lr, b, err = c.compileExpr0(ctx, a, b, s, x.Left, block) //, reg)
		if err != nil {
			return
		}

		rr, b, err = c.compileExpr0(ctx, a, b, s, x.Right, block) //, reg+1)
		if err != nil {
			return
		}

		b = fmt.Appendf(b, "	ADD	X%d, X%d, X%d	// expr %d\n", reg, lr, rr, e)
	default:
		return 0, nil, errors.New("unsupported expr: %T", x)
	}

	return reg, b, nil
}

func (s *state) addDep(b, dep int) {
	for _, d := range s.deps[b] {
		if dep == d {
			return
		}
	}

	s.deps[b] = append(s.deps[b], dep)
}

func (s *state) alloc(e ir.Expr) (reg int) {
	reg = s.regnext
	s.regnext++
	s.reg[e] = reg

	return reg
}

func phi2regs(x ir.Phi) (r []asm.Reg) {
	r = make([]asm.Reg, len(x))

	for i, x := range x {
		r[i] = asm.Reg(x)
	}

	return
}
*/

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

func newAArch64() *arch {
	a := &arch{
		freelist: make([]int, 16),
	}

	for i := range a.freelist {
		a.freelist[i] = len(a.freelist) - 1 - i
	}

	return a
}

func (a *arch) Alloc(id ir.Expr) int {
	l := len(a.freelist)

	if l == 0 {
		panic("no more")
	}

	reg := a.freelist[l-1]

	a.freelist = a.freelist[:l-1]

	tlog.Printw("alloc", "id", id, "reg", reg)

	return reg
}

func (a *arch) Free(reg int) {
	tlog.Printw("free", "id", tlog.None, "reg", reg)
	//	a.freelist = append(a.freelist, reg)
}

func (a *arch) Use(id ir.Expr, reg int) {
	for i, x := range a.freelist {
		if x == reg {
			tlog.Printw("use", "id", id, "reg", reg)
			l := len(a.freelist)
			copy(a.freelist[i:], a.freelist[i+1:])
			a.freelist = a.freelist[:l-1]
			return
		}
	}

	panic(reg)
}
