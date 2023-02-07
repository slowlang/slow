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

		exit *Scope

		definition int
		lab        ir.Label

		allScopes []*Scope
	}

	Scope struct {
		*pkgContext
		id ir.Expr
		f  *ir.Func

		cont *Scope
		brea *Scope

		def  map[string]int
		vars map[int]ir.Expr
		in   map[int]ir.Expr

		depth int
		prev  []*Scope

		phi  []ir.Expr
		code []ir.Expr

		retvals []ir.Expr

		delayphi bool

		from loc.PC
	}

	visitSet map[*Scope]struct{}
)

const (
	findNone = -1 - iota
	findVisited
	findDeadend
)

func (c *Front) Compile(ctx context.Context) (_ *ir.Package, err error) {
	p := &pkgContext{
		Package: &ir.Package{},
	}

	p.zero = p.alloc(ir.Zero{})

	s := rootScope(p)

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
	par.allScopes = par.allScopes[:0]

	tlog.Printw("compile func", "name", f.Name)

	s := par.nextScope(-1, 1, par)
	s.f = f

	lexit := s.label()
	s.exit = par.nextScope(lexit, -1)

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

	end.branchTo(s.exit)

	f.Out = make([]ir.Expr, len(f.Out))

	for i := range f.Out {
		phi := make(ir.Phi, 0, len(s.exit.prev))

		for _, p := range s.exit.prev {
			if p.retvals == nil {
				continue
			}

			tlog.Printw("ret phi", "prev", p.ptr(), "retvals", p.retvals)

			phi = append(phi, p.retvals[i])
		}

		f.Out[i] = s.exit.addphi(phi)
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

		s.fixPhi()
	}

	pr(s.exit)

	done = map[*Scope]struct{}{}

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

		args := []any{"p", s.ptr(), "prev", s.prevPtr(), "lab", s.idlabel(), "d", s.depth}
		if len(s.in) != 0 {
			args = append(args, "in", s.in)
		}
		if len(s.def) != 0 {
			args = append(args, "def", s.def)
		}
		if len(s.vars) != 0 {
			args = append(args, "vars", s.vars)
		}
		args = append(args, "from", s.from)

		tlog.Printw("scope", args...)
		//	tlog.Printw("scope", "p", s.ptr(), "prev", s.prevPtr(), "lab", s.idlabel(), "d", s.depth, "in", s.in, "def", s.def, "vars", s.vars, "from", tlog.FormatNext("%x"), s.from)

		if s.id >= 0 {
			x := s.Exprs[s.id]

			tlog.Printw("label", "id", s.id, "typ", tlog.NextIsType, x, "val", x)

			f.Code = append(f.Code, s.id)
		}

		for _, id := range s.phi {
			x := s.Exprs[id]

			tlog.Printw("phi", "id", id, "typ", tlog.NextIsType, x, "val", x)
		}

		f.Code = append(f.Code, s.phi...)

		for _, id := range s.code {
			x := s.Exprs[id]

			tlog.Printw("code", "id", id, "typ", tlog.NextIsType, x, "val", x)
		}

		f.Code = append(f.Code, s.code...)
	}

	tlog.Printw("function scopes", "name", f.Name)

	pr(s.exit)

	for _, s := range par.allScopes {
		if _, ok := done[s]; ok {
			continue
		}

		tlog.Printw("missed scopes", "p", s.ptr(), "", tlog.Error)

		pr(s)
	}

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
	case *ast.BranchStmt:
		switch x.Tok {
		case token.BREAK:
			s.doBreak()
		case token.CONTINUE:
			s.doContinue()
		default:
			panic(x)
		}
	default:
		panic(x)
	}

	return s, nil
}

func (c *Front) compileIf(ctx context.Context, prev *Scope, x *ast.IfStmt) (_ *Scope, err error) {
	var labels []ir.Label
	end := prev.label()

	scond := prev.nextScope(-1, 1, prev)
	next := prev.nextScope(end, 0)

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
			scond.branchTo(next)

			q = nil
		case *ast.IfStmt:
			q = qq
		default:
			panic(qq)
		}
	}

	for i := 0; ; {
		sub := scond.nextScope(labels[i], 1, scond)

		sub, err = c.compileBlock(ctx, sub, x.Body)
		if err != nil {
			return nil, errors.Wrap(err, "then")
		}

		sub.branchTo(next)

		i++

		switch xx := x.Else.(type) {
		case *ast.BlockStmt:
			sub := scond.nextScope(labels[i], 1, scond)

			sub, err = c.compileBlock(ctx, sub, xx)
			if err != nil {
				return nil, errors.Wrap(err, "else")
			}

			sub.branchTo(next)
		case nil:
			sub := scond.nextScope(-1, 1, scond)
			next.appendPrev(sub)
		case *ast.IfStmt:
			x = xx
			continue
		default:
			panic(xx)
		}

		break
	}

	tlog.Printw("if prev", "p", prev.ptr())
	tlog.Printw("if next", "p", next.ptr())

	return next, nil
}

func (c *Front) compileFor(ctx context.Context, prev *Scope, x *ast.ForStmt) (_ *Scope, err error) {
	lcond := prev.label()
	lbody := prev.label()
	lend := prev.label()

	var scond, sbody, send *Scope

	if x.Cond != nil {
		scond = prev.nextScope(lcond, 1, prev)
		sbody = prev.nextScope(lbody, 2, scond)
		send = prev.nextScope(lend, 0, scond)
	} else {
		scond = prev.nextScope(lcond, 1, prev)
		sbody = scond
		send = prev.nextScope(lend, 0)
	}

	for _, s := range []*Scope{scond, sbody, send} {
		if s == nil {
			continue
		}

		s := s

		s.delayphi = true

		defer func() {
			s.delayphi = false
		}()
	}

	sbody.cont = scond
	sbody.brea = send

	// cond

	prev.branchTo(scond)

	if x.Cond != nil {
		cond, condExpr, err := c.compileCond(ctx, scond, x.Cond)
		if err != nil {
			return nil, errors.Wrap(err, "cond")
		}

		scond.add(ir.BCond{
			Expr:  condExpr,
			Cond:  cond,
			Label: lbody,
		})

		scond.branchTo(send)
	}

	// body

	sbodyEnd, err := c.compileBlock(ctx, sbody, x.Body)
	if err != nil {
		return nil, errors.Wrap(err, "body")
	}

	sbodyEnd.branchTo(scond)

	tlog.Printw("for prev", "p", prev.ptr())
	tlog.Printw("for next", "p", sbodyEnd.ptr())

	return send, nil
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
			// TODO
			panic(e.Name)
			//	return id, errors.New("undefined var: %s", e)
		}
	case *ast.BasicLit:
		switch e.Kind {
		case token.INT:
			x, err := strconv.ParseUint(e.Value, 10, 64)
			if err != nil {
				return -1, errors.Wrap(err, "")
			}

			id = s.add(ir.Imm(x))
		case token.STRING:
			data, err := strconv.Unquote(e.Value)
			if err != nil {
				panic(err)
			}

			id = s.add(ir.Data(data))
			id = s.add(ir.Ptr{X: id})
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
	case *ast.IndexExpr:
		x, err := c.compileExpr(ctx, s, e.X)
		if err != nil {
			return -1, errors.Wrap(err, "index base")
		}

		idx, err := c.compileExpr(ctx, s, e.Index)
		if err != nil {
			return -1, errors.Wrap(err, "index base")
		}

		op := ir.Index{
			X: x,
			I: idx,
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

func rootScope(p *pkgContext) *Scope {
	s := &Scope{
		id:   -1,
		def:  map[string]int{},
		vars: map[int]ir.Expr{},
		in:   map[int]ir.Expr{},
		from: loc.Caller(1),

		pkgContext: p,
	}

	p.allScopes = append(p.allScopes, s)

	return s
}

func (par *Scope) nextScope(lab ir.Label, d int, prev ...*Scope) *Scope {
	s := rootScope(par.pkgContext)
	s.from = loc.Caller(1)
	s.prev = prev

	s.f = par.f
	s.depth = par.depth + d

	if lab >= 0 {
		s.id = s.alloc(lab)
	}

	tlog.Printw("new scope", "p", s.ptr(), "lab", s.idlabel(), "d", s.depth, "prev", s.prevPtr(), "from", loc.Callers(1, 3))

	return s
}

func (s *Scope) branchTo(to *Scope) {
	if l := len(s.code); l != 0 {
		if _, ok := s.Exprs[s.code[l-1]].(ir.B); ok {
			return
		}
	}

	s.add(ir.B{Label: to.idlabel()})

	to.appendPrev(s)
}

func (s *Scope) ret(ids ...ir.Expr) {
	tlog.Printw("ret", "s", s.id, "f", s.f)

	if len(s.f.Out) != len(ids) {
		panic("mismatch")
	}

	// TODO
	s.retvals = ids

	s.branchTo(s.exit)
}

func (s *Scope) doContinue() {
	q := s

	for q.cont == nil || q.depth > s.depth {
		q = q.prev[0]
	}

	tlog.Printw("continue", "from", s.ptr(), "to", q.cont.ptr(), "q", q.ptr())

	s.branchTo(q.cont)
}

func (s *Scope) doBreak() {
	q := s

	for q.brea == nil || q.depth > s.depth {
		q = q.prev[0]
	}

	tlog.Printw("break", "from", s.ptr(), "to", q.brea.ptr(), "q", q.ptr())

	s.branchTo(q.brea)
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

func (s *Scope) addphi(x ir.Phi) ir.Expr {
	id := s.pkgContext.alloc(x)

	s.phi = append(s.phi, id)

	return id
}

func (s *Scope) define(name string, id ir.Expr) {
	def := s.definition
	s.definition++

	tlog.Printw("define var", "p", s.ptr(), "lab", s.idlabel(), "d", s.depth, "name", name, "def", def, "id", id, "from", loc.Callers(1, 3))

	s.def[name] = def
	s.vars[def] = id
}

func (s *Scope) assign(name string, d int, id ir.Expr) {
	def := s.findDef(name, d)
	if def == -1 {
		panic(name)
	}

	tlog.Printw("assign var", "p", s.ptr(), "lab", s.idlabel(), "d", s.depth, "name", name, "def", def, "id", id, "from", loc.Callers(1, 3))

	s.vars[def] = id
}

func (s *Scope) findValue(name string, d int) (id ir.Expr) {
	var def int

	if tlog.If("findValue") {
		defer func() {
			tlog.Printw("find value", "p", s.ptr(), "lab", s.idlabel(), "d", s.depth, "name", name, "def", def, "id", id, "from", loc.Callers(1, 3))
		}()
	}

	def = s.findDef(name, d)
	if def == -1 {
		return -1
	}

	return s.findVar(def, nil)
}

func (s *Scope) findVar(def int, visited visitSet) (id ir.Expr) {
	if visited == nil {
		visited = make(visitSet)
	}

	if _, ok := visited[s]; ok {
		return findVisited
	}

	visited[s] = struct{}{}

	if tlog.If("findVar") {
		defer func() {
			tlog.Printw("find var", "p", s.ptr(), "lab", s.idlabel(), "d", s.depth, "def", def, "id", id, "from", loc.Callers(1, 3))
		}()
	}

	id, ok := s.vars[def]
	if ok {
		return id
	}

	id, ok = s.in[def]
	if ok {
		return id
	}

	if s.delayphi {
		id := s.addphi(ir.Phi{})

		s.in[def] = id

		return id
	}

	switch len(s.prev) {
	case 0:
		return -1
	case 1:
		return s.prev[0].findVar(def, visited)
	default:
	}

	phi := s.findMerge(def, visited)
	id = s.addphi(phi)

	s.in[def] = id

	return id
}

func (s *Scope) findMerge(def int, visited visitSet) ir.Phi {
	var phi ir.Phi

prevs:
	for _, p := range s.prev {
		sub := p.findVar(def, visited)
		switch sub {
		case findVisited, findDeadend:
			continue
		case findNone:
			panic(def)
		}

		if sub < 0 {
			panic(def)
		}

		for _, x := range phi {
			if sub == x {
				continue prevs
			}
		}

		phi = append(phi, sub)
	}

	if len(phi) == 0 {
		panic(def)
	}

	return phi
}

func (s *Scope) fixPhi() {
	for def, id := range s.in {
		x := s.Exprs[id].(ir.Phi)
		if len(x) != 0 {
			continue
		}

		s.Exprs[id] = s.findMerge(def, nil)
	}
}

func (s *Scope) findDef(name string, d int) (def int) {
	if tlog.If("findDef") {
		defer func() {
			tlog.Printw("find def", "p", s.ptr(), "lab", s.idlabel(), "d", s.depth, "name", name, "def", def, "req.d", d)
		}()
	}

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

func (s *Scope) appendPrev(prev *Scope) {
	for _, p := range s.prev {
		if p == prev {
			return
		}
	}

	s.prev = append(s.prev, prev)
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
