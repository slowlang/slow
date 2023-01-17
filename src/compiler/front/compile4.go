package front

import (
	"context"
	"fmt"
	"go/ast"
	"go/token"
	"strconv"
	"unsafe"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"

	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	pkgContext struct {
		*ir.Package

		zero       ir.Link
		funcByName map[string]ir.BlockID

		definition int
	}

	Scope struct {
		*pkgContext
		f  *ir.Func
		id ir.BlockID

		def  map[string]int
		vars map[int]ir.Link
		out  map[int]ir.Link

		varcb func(def int) ir.Link

		depth int
		prev  []*Scope

		from loc.PC
	}

	scopeList []*Scope
)

func (c *Front) Compile(ctx context.Context) (_ *ir.Package, err error) {
	p := &pkgContext{
		Package:    &ir.Package{},
		funcByName: map[string]ir.BlockID{},
	}

	p.zero = p.addlink(ir.Zero{})
	p.Self = p.add(p.Package)

	s := newScope(p.Self, 0)
	s.pkgContext = p

	for _, f := range c.files {
		for _, d := range f.Decls {
			switch x := d.(type) {
			case *ast.FuncDecl:
				f := &ir.Func{
					Name: x.Name.Name,
				}

				id := p.add(f)
				p.funcByName[f.Name] = id
				p.Funcs = append(p.Funcs, id)
			}
		}
	}

	for _, f := range c.files {
		//	tlog.Printw("file", "f", f)

		for _, d := range f.Decls {
			err = c.compileDecl(ctx, s, d)
			if err != nil {
				return nil, errors.Wrap(err, "decl %T", d)
			}
		}
	}

	return p.Package, nil
}

func (c *Front) compileDecl(ctx context.Context, s *Scope, d ast.Decl) (err error) {
	switch d := d.(type) {
	case *ast.FuncDecl:
		return c.compileFunc(ctx, s, d)
	default:
		panic(d)
	}
}

func (c *Front) compileFunc(ctx context.Context, par *Scope, fn *ast.FuncDecl) (err error) {
	fid := par.funcByName[fn.Name.Name]
	f := par.Blocks[fid].(*ir.Func)

	tlog.Printw("compile func", "name", f.Name)

	s := newScope(fid, 1, par)
	s.f = f

	f.Args = s.add(ir.Args{})
	args := 0

	for _, p := range fn.Type.Params.List {
		if len(p.Names) == 0 {
			//	f.Args = append(f.Args, 0)
			args++

			continue
		}

		for _, name := range p.Names {
			s.define(name.Name, ir.Link{Block: f.Args, Out: args})
			args++
			//	f.Args = append(f.Args, 0)
		}
	}

	for _, p := range fn.Type.Results.List {
		if len(p.Names) == 0 {
			f.Results = append(f.Results, ir.Nowhere)
			continue
		}

		for _, name := range p.Names {
			s.define(name.Name, s.zero)
			f.Results = append(f.Results, ir.Nowhere)
		}
	}

	end, err := c.compileBlock(ctx, s, fn.Body)
	if err != nil {
		return errors.Wrap(err, "body")
	}

	done := map[*Scope]struct{}{}

	var pr func(s *Scope)
	pr = func(s *Scope) {
		if s == nil {
			return
		}

		if _, ok := done[s]; ok {
			return
		}

		done[s] = struct{}{}

		for _, p := range s.prev {
			pr(p)
		}

		tlog.Printw("scope", "p", s.ptr(), "prev", s.prevPtr(), "id", s.id, "d", s.depth, "out", s.out, "def", s.def, "vars", s.vars, "from", tlog.FormatNext("%x"), s.from)
	}

	pr(end)

	return nil
}

func (c *Front) compileBlock(ctx context.Context, s *Scope, b *ast.BlockStmt) (_ *Scope, err error) {
	for _, x := range b.List {
		s, err = c.compileStmt(ctx, s, x)
		if err != nil {
			return nil, errors.Wrap(err, "")
		}
	}

	return s, nil
}

func (c *Front) compileStmt(ctx context.Context, s *Scope, x ast.Stmt) (_ *Scope, err error) {
	switch x := x.(type) {
	case *ast.ReturnStmt:
		ids := make([]ir.Link, len(x.Results))

		for i, e := range x.Results {
			ids[i], err = c.compileExpr(ctx, s, e)
			if err != nil {
				return nil, errors.Wrap(err, "return value")
			}
		}

		s.ret(ids...)
		// TODO

		return s, nil
	case *ast.AssignStmt:
		if len(x.Lhs) != len(x.Rhs) {
			panic("bad assign")
		}

		ids := make([]ir.Link, len(x.Rhs))

		for i, e := range x.Rhs {
			ids[i], err = c.compileExpr(ctx, s, e)
			if err != nil {
				return nil, errors.Wrap(err, "assignment rhs")
			}
		}

		for i, e := range x.Lhs {
			v, ok := e.(*ast.Ident)
			if !ok {
				return nil, errors.New("unsupported lexpr: %T", x.Lhs)
			}

			if x.Tok == token.DEFINE {
				s.define(v.Name, ids[i])
			} else {
				s.assign(v.Name, s.depth, ids[i])
			}
		}
	case *ast.IfStmt:
		s, err = c.compileIf(ctx, s, x)
		if err != nil {
			return s, errors.Wrap(err, "if")
		}
	case *ast.ForStmt:
		s, err = c.compileFor(ctx, s, x)
		if err != nil {
			return s, errors.Wrap(err, "for")
		}
	default:
		panic(x)
	}

	return s, nil
}

func (c *Front) compileIf(ctx context.Context, s *Scope, x *ast.IfStmt) (_ *Scope, err error) {
	b := &ir.Switch{}
	id := s.add(b)
	prev := s

	var branches []*Scope

	for {
		cond, condExpr, err := c.compileCond(ctx, prev, x.Cond)
		if err != nil {
			return nil, errors.Wrap(err, "if cond")
		}

		b.Preds = append(b.Preds, ir.Pred{
			Expr: condExpr,
			Cond: cond,
		})

		tup := prev.add(ir.Tuple{})
		sub := newScope(tup, 1, prev)

		sub, err = c.compileBlock(ctx, sub, x.Body)
		if err != nil {
			return nil, errors.Wrap(err, "then")
		}

		b.Blocks = append(b.Blocks, tup)
		branches = append(branches, sub)

		switch xx := x.Else.(type) {
		case *ast.BlockStmt:
			tup := prev.add(ir.Tuple{})
			sub := newScope(tup, 1, prev)

			sub, err = c.compileBlock(ctx, sub, xx)
			if err != nil {
				return nil, errors.Wrap(err, "else")
			}

			b.Blocks = append(b.Blocks, tup)
			branches = append(branches, sub)
		case *ast.IfStmt:
			x = xx
			continue
		case nil:
			tup := prev.add(ir.Tuple{})
			sub := newScope(tup, 1, prev)

			b.Blocks = append(b.Blocks, tup)
			branches = append(branches, sub)
		default:
			panic(xx)
		}

		break
	}

	s = newScope(id, -1, branches...)

	s.varcb = func(def int) ir.Link {
		var out int

		for _, sub := range branches {
			res := sub.findVar(def, s)
			if res == ir.Nowhere {
				panic(def)
			}

			tup := s.Blocks[sub.id].(ir.Tuple)
			out = len(tup)
			tup = append(tup, res)
			s.Blocks[sub.id] = tup
		}

		return ir.Link{
			Block: id,
			Out:   out,
		}
	}

	next := newScope(-1, 0, s)

	return next, nil
}

func (c *Front) compileFor(ctx context.Context, s *Scope, x *ast.ForStmt) (_ *Scope, err error) {
	b := &ir.Loop{}
	id := s.add(b)

	prev := s

	b.Body = s.add(ir.Tuple{})
	s = newScope(b.Body, 1, s)

	if x.Cond != nil {
		cond, condExpr, err := c.compileCond(ctx, s, x.Cond)
		if err != nil {
			return nil, errors.Wrap(err, "cond")
		}

		b.Cond = ir.Pred{
			Expr: condExpr,
			Cond: cond,
		}
	}

	s, err = c.compileBlock(ctx, s, x.Body)
	if err != nil {
		return nil, errors.Wrap(err, "body")
	}

	s = newScope(id, -1, s)

	for def, id := range prev.out {
		res := s.findVar(def, s)
		if id == res {
			continue
		}

		b.LoopIn = append(b.LoopIn, id)
		b.LoopOut = append(b.LoopOut, res)
	}

	tlog.Printw("loop", "prev.out", prev.out, "body.out", s.out, "body.vars", s.vars)

	s.varcb = func(def int) ir.Link {
		res := s.findVar(def, s)
		if res == ir.Nowhere {
			panic(def)
		}

		tup := s.Blocks[b.Body].(ir.Tuple)
		out := len(tup)
		tup = append(tup, res)
		s.Blocks[b.Body] = tup

		return ir.Link{
			Block: id,
			Out:   out,
		}
	}

	next := newScope(-1, 0, s)

	return next, nil
}

func (c *Front) compileCond(ctx context.Context, s *Scope, e ast.Expr) (cc ir.Cond, id ir.Link, err error) {
	switch e := e.(type) {
	case *ast.BinaryExpr:
		switch op := e.Op.String(); op {
		case "<", ">", "==", "!=", "<=", ">=":
			cc = ir.Cond(op)
		default:
			return "", ir.Nowhere, errors.New("unsupported op: %q", e.Op)
		}
	default:
		return "", ir.Nowhere, errors.New("unsupported expr: %T", e)
	}

	id, err = c.compileExpr(ctx, s, e)

	return
}

func (c *Front) compileExpr(ctx context.Context, s *Scope, e ast.Expr) (id ir.Link, err error) {
	switch e := e.(type) {
	case *ast.Ident:
		id = s.findValue(e.Name, s.depth)
		if id == ir.Nowhere {
			return id, errors.New("undefined var: %s", e)
		}
	case *ast.BasicLit:
		switch e.Kind {
		case token.INT:
			x, err := strconv.ParseUint(e.Value, 10, 64)
			if err != nil {
				return ir.Nowhere, errors.Wrap(err, "")
			}

			id = link(s.add(ir.Imm(x)))
		default:
			panic(e.Kind)
		}
	case *ast.BinaryExpr:
		l, err := c.compileExpr(ctx, s, e.X)
		if err != nil {
			return ir.Nowhere, errors.Wrap(err, "op lhs")
		}

		r, err := c.compileExpr(ctx, s, e.Y)
		if err != nil {
			return ir.Nowhere, errors.Wrap(err, "op rhs")
		}

		var op any

		switch e.Op.String() {
		case "+":
			op = ir.Add{
				L: l,
				R: r,
			}
		case "-":
			op = ir.Sub{
				L: l,
				R: r,
			}
		case "*":
			op = ir.Mul{
				L: l,
				R: r,
			}
		case "<", ">", "<=", ">=", "==", "!=":
			op = ir.Cmp{
				L: l,
				R: r,
			}
		default:
			panic(e.Op)
		}

		id = link(s.add(op))
	case *ast.CallExpr:
		n := e.Fun.(*ast.Ident)

		x := ir.Call{
			Func: link(s.getfunc(n.Name)),
			Args: make([]ir.Link, len(e.Args)),
		}

		for i, a := range e.Args {
			x.Args[i], err = c.compileExpr(ctx, s, a)
			if err != nil {
				return ir.Nowhere, errors.Wrap(err, "op lhs")
			}
		}

		id = link(s.add(x))
	default:
		panic(fmt.Sprintf("%T: %[1]v", e))
	}

	return
}

func newScope(id ir.BlockID, d int, prev ...*Scope) *Scope {
	s := &Scope{
		id:    id,
		def:   map[string]int{},
		vars:  map[int]ir.Link{},
		out:   map[int]ir.Link{},
		depth: d,
		prev:  prev,
		from:  loc.Caller(1),
	}

	if prev != nil {
		s.depth += prev[0].depth
		s.pkgContext = prev[0].pkgContext
	}

	tlog.Printw("new scope", "scope", s.ptr(), "prev", s.prevPtr(), "from", loc.Callers(1, 3))

	return s
}

func (s *Scope) sid() ir.BlockID {
	if s == nil {
		return -1
	}

	return s.id
}

func (s *Scope) ptr() uintptr {
	return uintptr(unsafe.Pointer(s)) & 0xffffff
}

func (s *Scope) prevPtr() []uintptr {
	if s == nil {
		return nil
	}

	l := make([]uintptr, len(s.prev))

	for i, p := range s.prev {
		l[i] = p.ptr()
	}

	return l
}

func (s *Scope) define(name string, id ir.Link) {
	def := s.definition
	s.definition++

	s.def[name] = def
	s.vars[def] = id
}

func (s *Scope) assign(name string, d int, id ir.Link) {
	def := s.findDef(name, d)
	if def == -1 {
		panic(name)
	}

	tlog.Printw("scope name", "scope", s.id, "name", name, "def", def, "id", id, "from", loc.Callers(1, 3))

	s.vars[def] = id
}

func (s *Scope) findValue(name string, d int) (id ir.Link) {
	def := s.findDef(name, d)
	if def == -1 {
		return ir.Nowhere
	}

	return s.findVar(def, s)
}

func (s *Scope) findVar(def int, src *Scope) (id ir.Link) {
	id, ok := s.out[def]
	if ok {
		return id
	}

	defer func() {
		if id != ir.Nowhere && s != src {
			s.out[def] = id
		}

		tlog.Printw("find var", "p", s.ptr(), "def", def, "src", src.ptr(), "id", id, "from", loc.Callers(1, 3))
	}()

	if s.varcb != nil && s != src {
		return s.varcb(def)
	}

	id, ok = s.vars[def]
	if ok {
		return id
	}

	switch len(s.prev) {
	case 0:
		return ir.Nowhere
	case 1:
		return s.prev[0].findVar(def, src)
	default:
		panic(def)
	}
}

func (s *Scope) findDef(name string, d int) int {
	if d >= s.depth {
		if def, ok := s.def[name]; ok {
			return def
		}
	}

	if len(s.prev) == 0 {
		return -1
	}

	// TODO: maybe check all prev?
	return s.prev[0].findDef(name, d)
}

func (s *Scope) ret(ids ...ir.Link) {
	if s.f == nil {
		s.prev[0].ret(ids...)
		return
	}

	tlog.Printw("ret", "s", s.id, "f", s.f)
	s.f.Results = ids
}

func (p *pkgContext) id() ir.BlockID {
	return ir.BlockID(len(p.Package.Blocks))
}

func (p *pkgContext) add(x ir.Block) ir.BlockID {
	id := p.id()
	p.Package.Blocks = append(p.Package.Blocks, x)

	return id
}

func (p *pkgContext) addlink(x ir.Block) ir.Link {
	return link(p.add(x))
}

func (p *pkgContext) getfunc(name string) ir.BlockID {
	id, ok := p.funcByName[name]
	if !ok {
		panic(name)
	}

	return id
}

func link(b ir.BlockID) ir.Link { return ir.Link{Block: b} }
