package back

import (
	"context"
	"sort"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/nikandfor/tlog/tlwire"

	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Arch interface{}

	Compiler struct {
	}

	pkgContext struct {
		Arch
		*ir.Package

		s    []stats
		q    []ir.BlockID
		code []any

		users Bitmap

		//	lab     Label
	}

	stats struct {
		Height int
		Used   int
		Outs   int `tlog:",hex"`

		Users Bitmap
		Uses  Bitmap
	}

	Bitmap struct {
		b  []uint64
		b0 [1]uint64
	}

	Label int

	B struct {
		Label Label
	}

	BCond struct {
		Expr  ir.Link
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

		//	visited: NewBitmap(),
	}

	tlog.Printw("package", "path", pkg.Path)

	err = c.compileBlock(ctx, p, link(pkg.Self))
	if err != nil {
		return nil, errors.Wrap(err, "")
	}

	for id, x := range p.Blocks {
		p.s[id].Uses.Clear(int(id))

		switch x := x.(type) {
		case *ir.Switch:
			for _, block := range x.Blocks {
				p.s[block].Outs |= p.s[id].Outs
			}
		}
	}

	for id, x := range p.Blocks {
		tlog.Printw("block", "id", id, "typ", tlog.FormatNext("%T"), x, "val", x, "stats", p.s[id])
	}

	for _, id := range pkg.Funcs {
		_ = id
		//	b, err = c.codegenBlock(ctx, b, p, link(id))
		//	if err != nil {
		//		return nil, errors.Wrap(err, "codegen func %d", id)
		//	}
	}

	return b, nil
}

func (c *Compiler) codegenBlock(ctx context.Context, p *pkgContext, l ir.Link) (err error) {
	b := l.Block
	x := p.Blocks[b]
	//	s := &p.s[l.Block]

	switch x := x.(type) {
	case *ir.Switch:
	case ir.Cmp:
	case ir.Add:
	case ir.Sub:
	case ir.Mul:
	case ir.Tuple:
	default:
		panic(x)
	}

	if err != nil {
		return errors.Wrap(err, "%T", 0)
	}

	return nil
}

func (c *Compiler) codegenFunc(ctx context.Context, p *pkgContext, b ir.BlockID, f *ir.Func) (err error) {
	return nil
}

func (c *Compiler) codegenSwitch(ctx context.Context, p *pkgContext, b ir.BlockID, x *ir.Switch) (err error) {
	base := len(p.code)
	_ = base

	return nil
}

func (c *Compiler) codegenFunc0(ctx context.Context, b []byte, p *pkgContext, fid ir.BlockID) (_ []byte, err error) {
	f := p.Blocks[fid].(*ir.Func)

	tlog.Printw("codegen func", "name", f.Name)

	p.q = p.q[:0]

	for id, bs := range p.s {
		id := ir.BlockID(id)

		if !bs.Users.IsSet(int(fid)) {
			continue
		}

		p.q = append(p.q, id)
	}

	sort.SliceStable(p.q, func(i, j int) bool {
		return p.s[p.q[i]].Height < p.s[p.q[j]].Height
	})

	for _, id := range p.q {
		x := p.Blocks[id]

		tlog.Printw("block", "id", id, "typ", tlog.FormatNext("%T"), x, "val", x)
	}

	return b, nil
}

func (c *Compiler) compileBlock(ctx context.Context, p *pkgContext, l ir.Link) (err error) {
	b := l.Block
	x := p.Blocks[b]
	s := &p.s[l.Block]

	s.Outs |= 1 << l.Out
	s.Used++
	s.Users.Or(p.users)
	s.Uses.Set(int(b))

	// TODO: move s.Outs through the switch in the end
	if s.Used > 1 {
		return nil
	}

	switch x := x.(type) {
	case ir.Args, ir.Zero, ir.Imm:
		s.Height = 1
	case *ir.Package:
		err = c.compilePackage(ctx, p, b, x)
	case *ir.Func:
		err = c.compileFunc(ctx, p, b, x)
	case *ir.Switch:
		err = c.compileSwitch(ctx, p, l, x)
	case *ir.Loop:
		err = c.compileLoop(ctx, p, l, x)
	case ir.Cmp:
		err = c.compileLinks(ctx, p, b, x.L, x.R)
	case ir.Add:
		err = c.compileLinks(ctx, p, b, x.L, x.R)
	case ir.Sub:
		err = c.compileLinks(ctx, p, b, x.L, x.R)
	case ir.Mul:
		err = c.compileLinks(ctx, p, b, x.L, x.R)
	case ir.Tuple:
		err = c.compileLinks(ctx, p, b, x...)
	default:
		panic(x)
	}

	if err != nil {
		return errors.Wrap(err, "%T", x)
	}

	return nil
}

func (c *Compiler) compilePackage(ctx context.Context, p *pkgContext, b ir.BlockID, pkg *ir.Package) (err error) {
	max := 0

	for _, block := range pkg.Funcs {
		err = c.compileBlock(ctx, p, link(block))
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

func (c *Compiler) compileFunc(ctx context.Context, p *pkgContext, b ir.BlockID, f *ir.Func) (err error) {
	//	tlog.Printw("compile func", "name", f.Name, "in", f.Args, "out", f.Results)

	defer p.addUser(b)()

	err = c.compileLinks(ctx, p, b, f.Results...)
	if err != nil {
		return errors.Wrap(err, "body")
	}

	return nil
}

func (c *Compiler) compileSwitch(ctx context.Context, p *pkgContext, l ir.Link, x *ir.Switch) (err error) {
	max := 0

	for i, pred := range x.Preds {
		err = c.compileBlock(ctx, p, pred.Expr)
		if err != nil {
			return errors.Wrap(err, "pred %d", i)
		}

		if h := p.s[pred.Expr.Block].Height; h > max {
			max = h
		}

		p.s[l.Block].Uses.Or(p.s[pred.Expr.Block].Uses)
	}

	for i, block := range x.Blocks {
		unuser := p.addUser(block)

		err = c.compileBlock(ctx, p, ir.Link{Block: block, Out: l.Out})
		unuser()
		if err != nil {
			return errors.Wrap(err, "block %d", i)
		}

		if h := p.s[block].Height; h > max {
			max = h
		}

		p.s[l.Block].Uses.Or(p.s[block].Uses)
	}

	p.s[l.Block].Height = max + 1

	return nil
}

func (c *Compiler) compileLoop(ctx context.Context, p *pkgContext, l ir.Link, x *ir.Loop) (err error) {
	return c.compileLinks(ctx, p, l.Block, x.Cond.Expr, ir.Link{Block: x.Body, Out: l.Out})
}

func (c *Compiler) compileLinks(ctx context.Context, p *pkgContext, b ir.BlockID, links ...ir.Link) (err error) {
	max := 0

	for _, l := range links {
		err = c.compileBlock(ctx, p, l)
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

/*
func (p *pkgContext) label() Label {
	l := p.lab
	p.lab++
	return l
}
*/

func NewBitmap() *Bitmap {
	s := MakeBitmap()
	return &s
}

func MakeBitmap() Bitmap {
	s := Bitmap{}
	s.b = s.b0[:]

	return s
}

func (s *Bitmap) Set(i int) {
	i, j := s.ij(i)

	s.grow(i)

	s.b[i] |= 1 << j
}

func (s Bitmap) Clear(i int) {
	i, j := s.ij(i)

	if i >= len(s.b) {
		return
	}

	s.b[i] &^= 1 << j
}

func (s Bitmap) IsSet(i int) bool {
	i, j := s.ij(i)

	if i >= len(s.b) {
		return false
	}

	return (s.b[i] & (1 << j)) != 0
}

func (s *Bitmap) Or(x Bitmap) {
	s.grow(len(x.b))

	for i, x := range x.b {
		s.b[i] |= x
	}
}

func (s Bitmap) And(x Bitmap) {
	for i, x := range x.b {
		if i == len(s.b) {
			break
		}

		s.b[i] &= x
	}
}

func (s Bitmap) AndNot(x Bitmap) {
	for i, x := range x.b {
		if i == len(s.b) {
			break
		}

		s.b[i] &^= x
	}
}

func (s Bitmap) AndNotCp(x Bitmap) Bitmap {
	cp := s.Copy()
	cp.AndNot(x)

	return cp
}

func (s Bitmap) Copy() Bitmap {
	r := MakeBitmap()
	r.Or(s)
	return r
}

func (s Bitmap) CopyPtr() *Bitmap {
	r := NewBitmap()
	r.Or(s)
	return r
}

func (s Bitmap) Reset() {
	for i := range s.b {
		s.b[i] = 0
	}
}

func (s Bitmap) Range(f func(i int) bool) {
	for i, x := range s.b {
		if x == 0 {
			continue
		}

		for j := 0; j < 64; j++ {
			if (x & (1 << j)) == 0 {
				continue
			}

			if !f(i*64 + j) {
				return
			}
		}
	}
}

func (s Bitmap) TlogAppend(b []byte) []byte {
	var e tlwire.LowEncoder

	b = e.AppendTag(b, tlwire.Array, -1)

	s.Range(func(i int) bool {
		b = e.AppendInt(b, i)

		return true
	})

	b = e.AppendBreak(b)

	return b
}

func (s Bitmap) ij(pos int) (i int, j int) {
	return pos / 64, pos % 64
}

func (s *Bitmap) grow(i int) {
	for i >= len(s.b) {
		s.b = append(s.b, 0)
	}
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
