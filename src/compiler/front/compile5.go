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

		zero ir.Expr
		exit ir.Label

		definition int
		lab        ir.Label
	}

	Scope struct {
		*pkgContext
		id ir.Expr
		f  *ir.Func

		def  map[string]int
		vars map[int]ir.Expr
		out  map[int]ir.Expr

		varcb func(def int) ir.Expr

		depth int
		prev  []*Scope

		code []ir.Expr

		from loc.PC
	}
)

func (c *Front) Compile(ctx context.Context) (_ *ir.Package, err error) {
	p := &pkgContext{
		Package: &ir.Package{},
	}

	p.zero = p.alloc(ir.Zero{})

	s := newScope(-1, 0)
	s.pkgContext = p

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
	f := &ir.Func{
		Name: fn.Name.Name,
	}

	par.Funcs = append(par.Funcs, f)

	tlog.Printw("compile func", "name", f.Name)

	s := newScope(-1, 1, par)
	s.f = f
	s.exit = s.label()

	argsStart := s.pkgContext.id()

	for _, p := range fn.Type.Params.List {
		if len(p.Names) == 0 {
			id := s.add(ir.Out(len(f.In)))
			f.In = append(f.In, ir.Param{
				Expr: id,
			})

			continue
		}

		for _, name := range p.Names {
			id := s.add(ir.Out(len(f.In)))
			f.In = append(f.In, ir.Param{
				Name: name.Name,
				Expr: id,
			})

			s.define(name.Name, id)
		}
	}

	if len(f.In) > 0 {
		s.Exprs[argsStart] = ir.Args(len(f.In))
	}

	for _, p := range fn.Type.Results.List {
		if len(p.Names) == 0 {
			f.Out = append(f.Out, -1)
			continue
		}

		for _, name := range p.Names {
			f.Out = append(f.Out, -1)

			s.define(name.Name, s.zero)
		}
	}

	end, err := c.compileBlock(ctx, s, fn.Body)
	if err != nil {
		return errors.Wrap(err, "body")
	}

	exit := newScope(s.exit, 0, end)

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

		tlog.Printw("scope", "p", s.ptr(), "prev", s.prevPtr(), "lab", s.idlabel(), "d", s.depth, "out", s.out, "def", s.def, "vars", s.vars, "from", tlog.FormatNext("%x"), s.from)

		for _, id := range s.code {
			x := s.Exprs[id]

			tlog.Printw("func code", "id", id, "typ", tlog.NextType, x, "val", x)
		}

		if s.id >= 0 {
			f.Code = append(f.Code, s.id)
		}

		f.Code = append(f.Code, s.code...)
	}

	pr(exit)

	/*
		for _, id := range f.Code {
			x := s.Exprs[id]

			tlog.Printw("func code", "id", id, "typ", tlog.NextType, x, "val", x)
		}
	*/

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
		ids := make([]ir.Expr, len(x.Results))

		for i, e := range x.Results {
			ids[i], err = c.compileExpr(ctx, s, e)
			if err != nil {
				return nil, errors.Wrap(err, "return value")
			}
		}

		s.ret(ids...)

		return s, nil
	case *ast.AssignStmt:
		if len(x.Lhs) != len(x.Rhs) {
			panic("bad assign")
		}

		ids := make([]ir.Expr, len(x.Rhs))

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

func (c *Front) compileIf(ctx context.Context, prev *Scope, x *ast.IfStmt) (_ *Scope, err error) {
	scond := newScope(-1, 1, prev)

	var labels []ir.Label
	end := prev.label()

	for q := x; q != nil; {
		cond, condExpr, err := c.compileCond(ctx, scond, x.Cond)
		if err != nil {
			return nil, errors.Wrap(err, "if cond")
		}

		lab := prev.label()
		labels = append(labels, lab)

		scond.add(ir.BCond{
			Expr:  condExpr,
			Cond:  cond,
			Label: lab,
		})

		switch qq := q.Else.(type) {
		case *ast.BlockStmt:
			lab := prev.label()
			labels = append(labels, lab)

			scond.add(ir.B{
				Label: lab,
			})

			q = nil
		case nil:
			scond.add(ir.B{
				Label: end,
			})

			q = nil
		case *ast.IfStmt:
			q = qq
		default:
			panic(qq)
		}
	}

	var branches []*Scope

	for i := 0; ; {
		sub := newScope(labels[i], 1, scond)

		sub, err = c.compileBlock(ctx, sub, x.Body)
		if err != nil {
			return nil, errors.Wrap(err, "then")
		}

		sub.add(ir.B{Label: end})

		branches = append(branches, sub)

		i++

		switch xx := x.Else.(type) {
		case *ast.BlockStmt:
			sub := newScope(labels[i], 1, scond)

			sub, err = c.compileBlock(ctx, sub, xx)
			if err != nil {
				return nil, errors.Wrap(err, "else")
			}

			sub.add(ir.B{Label: end})

			branches = append(branches, sub)
		case nil:
			sub := newScope(-1, 1, scond)

			branches = append(branches, sub)
		case *ast.IfStmt:
			x = xx
			continue
		default:
			panic(xx)
		}

		break
	}

	s := newScope(end, -2, branches...)

	s.varcb = func(def int) ir.Expr {
		phi := make(ir.Phi, len(branches))

		for i, sub := range branches {
			res := sub.findVar(def, s)
			if res == -1 {
				panic(def)
			}

			phi[i] = res
		}

		return s.add(phi)
	}

	next := newScope(-1, 0, s)

	return next, nil
}

func (c *Front) compileFor(ctx context.Context, s *Scope, x *ast.ForStmt) (_ *Scope, err error) {
	prev := s
	depth := 1

	var cond ir.Label
	end := s.label()
	loop := s.label()

	if x.Cond != nil {
		cond = s.label()
		s.add(ir.B{Label: cond})

		s = newScope(cond, 1, s)
		depth++

		cond, condExpr, err := c.compileCond(ctx, s, x.Cond)
		if err != nil {
			return nil, errors.Wrap(err, "cond")
		}

		s.add(ir.BCond{
			Expr:  condExpr,
			Cond:  cond,
			Label: loop,
		})

		s.add(ir.B{Label: end})
	}

	s = newScope(loop, 1, s)

	s, err = c.compileBlock(ctx, s, x.Body)
	if err != nil {
		return nil, errors.Wrap(err, "body")
	}

	if x.Cond != nil {
		s.add(ir.B{Label: cond})
	} else {
		s.add(ir.B{Label: loop})
	}

	s = newScope(end, -depth, s)

	for def, id := range prev.out {
		res := s.findVar(def, s)
		if id == res {
			continue
		}

		//	b.LoopIn = append(b.LoopIn, id)
		//	b.LoopOut = append(b.LoopOut, res)
	}

	tlog.Printw("loop", "prev.out", prev.out, "body.out", s.out, "body.vars", s.vars)

	s.varcb = func(def int) ir.Expr {
		res := s.findVar(def, s)
		if res == -1 {
			panic(def)
		}

		return res
	}

	next := newScope(-1, 0, s)

	return next, nil
}

func (c *Front) compileCond(ctx context.Context, s *Scope, e ast.Expr) (cc ir.Cond, id ir.Expr, err error) {
	switch e := e.(type) {
	case *ast.BinaryExpr:
		switch op := e.Op.String(); op {
		case "<", ">", "==", "!=", "<=", ">=":
			cc = ir.Cond(op)
		default:
			return "", -1, errors.New("unsupported op: %q", e.Op)
		}
	default:
		return "", -1, errors.New("unsupported expr: %T", e)
	}

	id, err = c.compileExpr(ctx, s, e)

	return
}

func (c *Front) compileExpr(ctx context.Context, s *Scope, e ast.Expr) (id ir.Expr, err error) {
	switch e := e.(type) {
	case *ast.Ident:
		id = s.findValue(e.Name, s.depth)
		if id == -1 {
			return id, errors.New("undefined var: %s", e)
		}
	case *ast.BasicLit:
		switch e.Kind {
		case token.INT:
			x, err := strconv.ParseUint(e.Value, 10, 64)
			if err != nil {
				return -1, errors.Wrap(err, "")
			}

			id = s.add(ir.Imm(x))
		default:
			panic(e.Kind)
		}
	case *ast.BinaryExpr:
		l, err := c.compileExpr(ctx, s, e.X)
		if err != nil {
			return -1, errors.Wrap(err, "op lhs")
		}

		r, err := c.compileExpr(ctx, s, e.Y)
		if err != nil {
			return -1, errors.Wrap(err, "op rhs")
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

		id = s.add(op)
	case *ast.CallExpr:
		n := e.Fun.(*ast.Ident)

		x := ir.Call{
			Func: n.Name,
			In:   make([]ir.Expr, len(e.Args)),
		}

		for i, a := range e.Args {
			x.In[i], err = c.compileExpr(ctx, s, a)
			if err != nil {
				return -1, errors.Wrap(err, "op lhs")
			}
		}

		id = s.add(x)

		// TODO: alloc regs for all results
	//	for i := 1; i < len(x.Out); i++ {
	//		s.add(ir.Out(i))
	//	}
	default:
		panic(fmt.Sprintf("%T: %[1]v", e))
	}

	return
}

func newScope(lab ir.Label, d int, prev ...*Scope) *Scope {
	s := &Scope{
		id:    -1,
		def:   map[string]int{},
		vars:  map[int]ir.Expr{},
		out:   map[int]ir.Expr{},
		depth: d,
		prev:  prev,
		from:  loc.Caller(1),
	}

	if prev != nil {
		s.depth += prev[0].depth
		s.pkgContext = prev[0].pkgContext
		s.f = prev[0].f
	}

	if lab >= 0 {
		s.id = s.add(lab)
	}

	tlog.Printw("new scope", "scope", s.ptr(), "prev", s.prevPtr(), "lab", s.idlabel(), "from", loc.Callers(1, 3))

	return s
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

func (s *Scope) idlabel() ir.Label {
	if s == nil || s.id == -1 || s.pkgContext == nil {
		return -1
	}

	return s.Exprs[s.id].(ir.Label)
}

func (s *Scope) add(x any) ir.Expr {
	id := s.pkgContext.alloc(x)

	s.code = append(s.code, id)

	return id
}

func (s *Scope) define(name string, id ir.Expr) {
	def := s.definition
	s.definition++

	s.def[name] = def
	s.vars[def] = id
}

func (s *Scope) assign(name string, d int, id ir.Expr) {
	def := s.findDef(name, d)
	if def == -1 {
		panic(name)
	}

	tlog.Printw("scope name", "scope", s.id, "name", name, "def", def, "id", id, "from", loc.Callers(1, 3))

	s.vars[def] = id
}

func (s *Scope) findValue(name string, d int) (id ir.Expr) {
	def := s.findDef(name, d)
	if def == -1 {
		return -1
	}

	return s.findVar(def, s)
}

func (s *Scope) findVar(def int, src *Scope) (id ir.Expr) {
	id, ok := s.out[def]
	if ok {
		return id
	}

	defer func() {
		if id != -1 && s != src {
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
		return -1
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

func (s *Scope) ret(ids ...ir.Expr) {
	if s.f == nil {
		s.prev[0].ret(ids...)
		return
	}

	tlog.Printw("ret", "s", s.id, "f", s.f)

	if len(s.f.Out) != len(ids) {
		panic("mismatch")
	}

	s.f.Out = ids
}

func (p *pkgContext) id() ir.Expr {
	return ir.Expr(len(p.Package.Exprs))
}

func (p *pkgContext) alloc(x any) ir.Expr {
	id := ir.Expr(len(p.Package.Exprs))
	p.Package.Exprs = append(p.Package.Exprs, x)

	return id
}

func (p *pkgContext) label() ir.Label {
	l := p.lab
	p.lab++

	return l
}
