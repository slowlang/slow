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

	BL	main

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
	tlog.Printw("func", "name", f.Name, "in", f.In, "out", f.Out)

	for block := range f.Blocks {
		b := f.Blocks[block]

		for _, p := range b.Phi {
			tlog.Printw("block", "id", "__", "block", block, "next", b.Next, "phi", p, "exprs", f.Exprs[p])
		}
	}

	for _, p := range f.In {
		e := f.Exprs[p.Expr]
		tlog.Printw("arg", "id", p.Expr, "block", "_", "type", tlog.FormatNext("%T"), e, "val", e)
	}

	printBlock := func(block int) {
		b := f.Blocks[block]

		for _, id := range b.Phi {
			e := f.Exprs[id]
			tlog.Printw("phi", "id", id, "block", block, "type", tlog.FormatNext("%T"), e, "val", e)
		}

		for _, id := range b.Code {
			e := f.Exprs[id]
			tlog.Printw("expr", "id", id, "block", block, "type", tlog.FormatNext("%T"), e, "val", e)
		}

		next := func() interface{} {
			if b.Next >= 0 {
				return b.Next
			}

			return "_"
		}

		tlog.Printw("next", "id", "__", "block", block, "next", next())
	}

	for block := 1; block < len(f.Blocks); block++ {
		printBlock(block)
	}

	printBlock(0)

	regmap := map[ir.Expr]int{}

	for i, p := range f.In {
		regmap[p.Expr] = i
	}

	allocBlock := func(block int) {
	}

	allocBlock(0)

	tlog.Printw("regmap", "map", regmap)

	return b, nil
}

/*
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
