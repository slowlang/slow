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

		s []stats
		//	q    []ir.BlockID
		code []any

		users   bitmap.Big
		visited bitmap.Big

		lab Label
	}

	stats struct {
		Height int
		Used   int
		Outs   int `tlog:",hex"`

		Users bitmap.Big
		Uses  bitmap.Big
	}

	Label int

	B struct {
		Label Label
	}

	BCond struct {
		Expr  ir.Reg
		Cond  ir.Cond
		Label Label
	}
)

func New() *Compiler {
	return nil
}

func (c *Compiler) CompilePackage(ctx context.Context, a Arch, b []byte, pkg *ir.Package) (_ []byte, err error) {
	p := &pkgContext{
		Arch:    a,
		Package: pkg,

		//	q: make([]ir.BlockID, 0, len(p.Blocks)),
		s: make([]stats, len(pkg.Blocks)),
	}

	tlog.Printw("package", "path", pkg.Path)

	err = c.analyzeBlock(ctx, p, link(pkg.Self))
	if err != nil {
		return nil, errors.Wrap(err, "")
	}

	/*
		for id, x := range p.Blocks {
			p.s[id].Uses.Clear(int(id))

			switch x := x.(type) {
			case *ir.Switch:
				for _, block := range x.Blocks {
					p.s[block].Outs |= p.s[id].Outs
				}
			}
		}
	*/

	for id, x := range p.Blocks {
		tlog.Printw("block", "id", id, "typ", tlog.FormatNext("%T"), x, "val", x, "stats", p.s[id])
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
`, p.Path)

	for _, id := range pkg.Funcs {
		f := p.Blocks[id].(*ir.Func)

		p.visited.Reset()
		p.code = p.code[:0]

		err = c.iselectBlock(ctx, p, id)
		if err != nil {
			return nil, errors.Wrap(err, "iselect func %d", id)
		}

		tlog.Printw("func code", "name", f.Name)

		for _, x := range p.code {
			tlog.Printw("code", "typ", tlog.NextType, x, "val", x)
		}

		b, err = c.codegenFunc(ctx, b, p, id, f)
		if err != nil {
			return nil, errors.Wrap(err, "codegen func %d", id)
		}
	}

	return b, nil
}

func (c *Compiler) codegenFunc(ctx context.Context, b []byte, p *pkgContext, block ir.BlockID, f *ir.Func) (_ []byte, err error) {
	b = fmt.Appendf(b, `
.global _%v
.align 4
_%[1]v:
	STP     FP, LR, [SP, #-16]!
	MOV     FP, SP

`, f.Name)

	b = fmt.Appendf(b, `
	LDP     FP, LR, [SP], #16
	RET
`)

	return b, nil
}

func (c *Compiler) iselectBlock(ctx context.Context, p *pkgContext, b ir.BlockID) (err error) {
	//	b := l.Block
	x := p.Blocks[b]
	//	s := &p.s[l.Block]

	if p.visited.IsSet(int(b)) {
		return nil
	}

	p.visited.Set(int(b))

	switch x := x.(type) {
	case ir.Args, ir.Imm:
		p.code = append(p.code, x)
	case *ir.Func:
		err = c.iselectFunc(ctx, p, b, x)
	case *ir.Switch:
		err = c.iselectSwitch(ctx, p, b, x)
	case *ir.Loop:
		err = c.iselectLoop(ctx, p, b, x)
	case ir.Cmp:
		err = c.iselectOp(ctx, p, x, x.L, x.R)
	case ir.Add:
		err = c.iselectOp(ctx, p, x, x.L, x.R)
	case ir.Sub:
		err = c.iselectOp(ctx, p, x, x.L, x.R)
	case ir.Mul:
		err = c.iselectOp(ctx, p, x, x.L, x.R)
	case ir.Tuple:
		err = c.iselectLinks(ctx, p, x...)
	default:
		panic(x)
	}

	if err != nil {
		return errors.Wrap(err, "%T", 0)
	}

	return nil
}

func (c *Compiler) iselectFunc(ctx context.Context, p *pkgContext, b ir.BlockID, f *ir.Func) (err error) {
	return c.iselectLinks(ctx, p, f.Results...)
}

func (c *Compiler) iselectSwitch(ctx context.Context, p *pkgContext, b ir.BlockID, x *ir.Switch) (err error) {
	base := len(p.code)
	_ = base

	for _, id := range x.Context {
		err = c.iselectBlock(ctx, p, p.Regs[id].Block)
		if err != nil {
			return err
		}
	}

	labels := make([]Label, len(x.Blocks))
	end := p.label()

	for i, pred := range x.Preds {
		err = c.iselectBlock(ctx, p, p.Regs[pred.Expr].Block)
		if err != nil {
			return err
		}

		labels[i] = p.label()

		p.code = append(p.code, BCond{
			Expr:  pred.Expr,
			Cond:  pred.Cond,
			Label: labels[i],
		})
	}

	last := len(labels) - 1
	labels[last] = p.label()

	p.code = append(p.code, B{
		Label: labels[last],
	})

	for i, id := range x.Blocks {
		p.code = append(p.code, labels[i])

		err = c.iselectBlock(ctx, p, id)
		if err != nil {
			return err
		}

		p.code = append(p.code, B{
			Label: end,
		})
	}

	p.code = append(p.code, end)

	return nil
}

func (c *Compiler) iselectLoop(ctx context.Context, p *pkgContext, b ir.BlockID, x *ir.Loop) (err error) {
	base := len(p.code)
	_ = base

	return nil
}

func (c *Compiler) iselectOp(ctx context.Context, p *pkgContext, x any, regs ...ir.Reg) (err error) {
	err = c.iselectLinks(ctx, p, regs...)
	if err != nil {
		return err
	}

	p.code = append(p.code, x)

	return nil
}

func (c *Compiler) iselectLinks(ctx context.Context, p *pkgContext, regs ...ir.Reg) (err error) {
	for _, x := range regs {
		err = c.iselectBlock(ctx, p, p.Regs[x].Block)
		if err != nil {
			return err
		}
	}

	return nil
}

func (c *Compiler) analyzeBlock(ctx context.Context, p *pkgContext, l ir.Link) (err error) {
	b := l.Block
	x := p.Blocks[b]
	s := &p.s[b]

	s.Outs |= 1 << l.Out
	s.Used++
	s.Users.Or(p.users)
	s.Uses.Set(int(b))

	if p.visited.IsSet(int(b)) {
		return nil
	}

	p.visited.Set(int(b))

	switch x := x.(type) {
	case ir.Args, ir.Zero, ir.Imm:
		s.Height = 1
	case *ir.Package:
		err = c.analyzePackage(ctx, p, b, x)
	case *ir.Func:
		err = c.analyzeFunc(ctx, p, b, x)
	case *ir.Switch:
		err = c.analyzeSwitch(ctx, p, l, x)
	case *ir.Loop:
		err = c.analyzeLoop(ctx, p, l, x)
	case ir.Cmp:
		err = c.analyzeRegs(ctx, p, b, x.L, x.R)
	case ir.Add:
		err = c.analyzeRegs(ctx, p, b, x.L, x.R)
	case ir.Sub:
		err = c.analyzeRegs(ctx, p, b, x.L, x.R)
	case ir.Mul:
		err = c.analyzeRegs(ctx, p, b, x.L, x.R)
	case ir.Tuple:
		err = c.analyzeRegs(ctx, p, b, x...)
	default:
		panic(x)
	}

	if err != nil {
		return errors.Wrap(err, "%T", x)
	}

	return nil
}

func (c *Compiler) analyzePackage(ctx context.Context, p *pkgContext, b ir.BlockID, pkg *ir.Package) (err error) {
	max := 0

	for _, block := range pkg.Funcs {
		err = c.analyzeBlock(ctx, p, link(block))
		if err != nil {
			return errors.Wrap(err, "func %v", block)
		}

		if h := p.s[block].Height; h > max {
			max = h
		}
	}

	p.s[b].Height = max + 1

	return nil
}

func (c *Compiler) analyzeFunc(ctx context.Context, p *pkgContext, b ir.BlockID, f *ir.Func) (err error) {
	//	tlog.Printw("analyze func", "name", f.Name, "in", f.Args, "out", f.Results)

	defer p.addUser(b)()

	err = c.analyzeRegs(ctx, p, b, f.Results...)
	if err != nil {
		return errors.Wrap(err, "body")
	}

	return nil
}

func (c *Compiler) analyzeSwitch(ctx context.Context, p *pkgContext, l ir.Link, x *ir.Switch) (err error) {
	max := 0

	for i, pred := range x.Preds {
		pl := p.Regs[pred.Expr]

		err = c.analyzeBlock(ctx, p, pl)
		if err != nil {
			return errors.Wrap(err, "pred %d", i)
		}

		if h := p.s[pl.Block].Height; h > max {
			max = h
		}

		p.s[l.Block].Uses.Or(p.s[pl.Block].Uses)
	}

	var context bitmap.Big
	prev := p.visited.Copy()

	for i, block := range x.Blocks {
		p.users.Set(int(block))

		err = c.analyzeBlock(ctx, p, ir.Link{Block: block, Out: l.Out})
		if err != nil {
			return errors.Wrap(err, "block %d", i)
		}

		if h := p.s[block].Height; h > max {
			max = h
		}

		p.users.Clear(int(block))
		p.s[l.Block].Uses.Or(p.s[block].Uses)
		p.s[block].Uses.Clear(int(block))
		p.s[block].Users.Clear(int(block))

		tlog.Printw("branch context", "id", l.Block, "br", i, "b", block, "uses", p.s[block].Uses)

		if i == 0 {
			context.Or(p.s[block].Uses)
		} else {
			context.And(p.s[block].Uses)
		}
	}

	tlog.Printw("switch context", "id", l.Block, "context", context, "noprev", context.AndNotCp(prev))

	context.Range(func(i int) bool {
		//	x.Context = append(x.Context, link(ir.BlockID(i)))

		return true
	})

	p.s[l.Block].Height = max + 1

	return nil
}

func (c *Compiler) analyzeLoop(ctx context.Context, p *pkgContext, l ir.Link, x *ir.Loop) (err error) {
	return c.analyzeLinks(ctx, p, l.Block, p.Regs[x.Cond.Expr], ir.Link{Block: x.Body, Out: l.Out})
}

func (c *Compiler) analyzeRegs(ctx context.Context, p *pkgContext, b ir.BlockID, regs ...ir.Reg) (err error) {
	links := make([]ir.Link, len(regs))

	for i, reg := range regs {
		links[i] = p.Regs[reg]
	}

	return c.analyzeLinks(ctx, p, b, links...)
}

func (c *Compiler) analyzeLinks(ctx context.Context, p *pkgContext, b ir.BlockID, links ...ir.Link) (err error) {
	max := 0

	for _, l := range links {
		err = c.analyzeBlock(ctx, p, l)
		if err != nil {
			return errors.Wrap(err, "%v", l)
		}

		if h := p.s[l.Block].Height; h > max {
			max = h
		}

		p.s[b].Uses.Or(p.s[l.Block].Uses)
	}

	p.s[b].Height = max + 1

	return nil
}

func (p *pkgContext) addUser(b ir.BlockID) func() {
	p.users.Set(int(b))

	return func() {
		p.users.Clear(int(b))
	}
}

func (p *pkgContext) label() Label {
	l := p.lab
	p.lab++
	return l
}

func cond2asm(cond ir.Cond) string {
	switch cond {
	case "<":
		return "LT"
	case ">":
		return "GT"
	case "<=":
		return "LE"
	case ">=":
		return "GE"
	case "==":
		return "EQ"
	case "!=":
		return "NE"
	default:
		panic(cond)
	}
}

func link(b ir.BlockID) ir.Link { return ir.Link{Block: b} }
