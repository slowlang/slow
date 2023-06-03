package front

import (
	"context"
	"fmt"
	"go/ast"
	"go/token"
	"path"
	"strconv"
	"unsafe"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"
	"github.com/nikandfor/tlog/tlwire"
	"github.com/slowlang/slow/src/compiler/ir"
	"github.com/slowlang/slow/src/compiler/tp"
)

type (
	predefined struct {
		untyped ir.Type
		typeDef ir.Type

		cmpt ir.Type

		zero ir.Expr

		Int ir.Type
	}

	pkgContext struct {
		*ir.Package

		predefined

		root *Scope

		nextdef   definition
		nextlabel ir.Label

		queue map[string]any
	}

	funContext struct {
		*ir.Func

		exit *Scope
	}

	Scope struct {
		*pkgContext
		*funContext

		defs map[string]definition
		vars map[definition]ir.Expr

		breakDst, continueDst *Scope

		depth int
		prev  []*Scope

		lab   ir.Label
		labid ir.Expr

		phi  []ir.Expr
		code []ir.Expr

		retids []ir.Expr

		valuef  func(definition, visitSet) ir.Expr
		statef  func(visitSet) ir.State
		effectf func(visitSet) ir.Effect

		from loc.PC
	}

	definition int

	visitSet map[*Scope]struct{}
)

const (
	walkNotFound = -1 - iota
	walkCycled
)

func (c *Front) Compile(ctx context.Context) (_ *ir.Package, err error) {
	tr, ctx := tlog.SpawnFromContextAndWrap(ctx, "front: compile package", "name", func() any {
		for _, f := range c.files {
			return f.Name.Name
		}

		return tlwire.Nil
	}())
	defer tr.Finish("err", &err)

	p := &pkgContext{
		Package: &ir.Package{},
		queue:   make(map[string]any),
	}

	p.untyped = ir.Type(p.alloc(tp.Untyped{}, 0))
	p.typeDef = ir.Type(p.alloc(tp.TypeDef{}, p.untyped))

	p.cmpt = p.addType(tp.Cmp{})
	p.Int = p.addType(tp.Int{Signed: true})

	s := rootScope(p, nil)
	s.from = loc.Caller(0)
	p.root = s

	for _, f := range c.files {
		if p.Path != "" && p.Path != f.Name.Name {
			return nil, errors.New("package name mispatch: %v and %v", p.Path, f.Name.Name)
		}

		p.Path = f.Name.Name

		for _, d := range f.Decls {
			err = c.addDecl(ctx, p, d)
			if err != nil {
				return nil, errors.Wrap(err, "add decl")
			}
		}
	}

	for name, d := range p.queue {
		delete(p.queue, name)

		err = c.compileDecl(ctx, p.root, d)
		if err != nil {
			return nil, errors.Wrap(err, "compile decl %v", name)
		}
	}

	return p.Package, nil
}

func (c *Front) addDecl(ctx context.Context, p *pkgContext, d ast.Decl) error {
	add := func(name string) error {
		if _, ok := p.queue[name]; ok {
			return errors.New("name redefined: %v", name)
		}

		p.queue[name] = d

		return nil
	}

	switch d := d.(type) {
	case *ast.FuncDecl:
		return add(d.Name.Name)
	case *ast.GenDecl:
		for _, spec := range d.Specs {
			switch d := spec.(type) {
			case *ast.TypeSpec:
				err := add(d.Name.Name)
				if err != nil {
					return err
				}
			default:
				panic(d)
			}
		}
	default:
		panic(d)
	}

	return nil
}

func (c *Front) compileDecl(ctx context.Context, s *Scope, d any) error {
	switch d := d.(type) {
	case *ast.FuncDecl:
		_, err := c.compileFunc(ctx, s, d)
		return err
	default:
		panic(d)
	}
}

func (c *Front) compileFunc(ctx context.Context, par *Scope, d *ast.FuncDecl) (fid ir.Expr, err error) {
	tr, ctx := tlog.SpawnFromContextAndWrap(ctx, "compile function", "name", d.Name.Name, "scope", par.ptr())
	defer tr.Finish("id", &fid, "err", &err)

	f := &ir.Func{
		Name: d.Name.Name,
	}
	ftp := &tp.Func{}

	s := par.nextScope(-1, 1, par)
	s.funContext = &funContext{
		Func: f,
	}

	for _, p := range d.Type.Params.List {
		tpid, err := c.compileType(ctx, par, p.Type)
		if err != nil {
			return -1, errors.Wrap(err, "param type")
		}

		addParam := func() ir.Expr {
			id := s.add(ir.Out(len(f.In)), tpid)
			f.In = append(f.In, id)
			ftp.In = append(ftp.In, tpid)

			return id
		}

		if len(p.Names) == 0 {
			addParam()
		}

		for _, name := range p.Names {
			id := addParam()
			s.define(name.Name, id)
		}
	}

	if len(f.In) != 0 {
		par.Exprs[f.In[0]] = ir.Args(len(f.In))
	}

	for _, p := range d.Type.Results.List {
		tpid, err := c.compileType(ctx, par, p.Type)
		if err != nil {
			return -1, errors.Wrap(err, "return param type")
		}

		addParam := func() {
			f.Out = append(f.Out, -1)
			ftp.Out = append(ftp.Out, tpid)
		}

		if len(p.Names) == 0 {
			addParam()
		}

		for _, name := range p.Names {
			addParam()
			s.define(name.Name, s.zero)
		}
	}

	tpid := par.addType(ftp)
	fid = par.alloc(f, tpid)

	par.define(f.Name, fid)
	par.Funcs = append(par.Funcs, fid)

	s.exit = s.nextScope(s.label(), 0)

	end, err := c.compileBlock(ctx, s, d.Body)
	if err != nil {
		return fid, errors.Wrap(err, "body")
	}

	end.branchTo(s.exit)

	for i := range f.Out {
		var phi ir.Phi

		for _, p := range s.exit.prev {
			b := p.finalBranch()

			phi = append(phi, ir.PhiBranch{
				B:    b,
				Expr: p.retids[i],
			})
		}

		f.Out[i] = s.exit.addphi(phi, s.EType[phi[0].Expr])
	}

	fmtFrom := func(pc loc.PC) string {
		if pc == 0 {
			return ""
		}

		name, _, line := pc.NameFileLine()
		name = path.Ext(name)

		return fmt.Sprintf("%s:%d", name, line)
	}

	printScope := func(s *Scope) {
		args := []interface{}{"ptr", s.ptr(), "d", s.depth}
		args = append(args, "from", fmtFrom(s.from))
		args = append(args, "prev", s.prevPtr())

		if s.lab != -1 {
			args = append(args, "lab", s.lab)
		}

		if len(s.defs) != 0 {
			args = append(args, "defs", s.defs)
		}
		if len(s.vars) != 0 {
			args = append(args, "vars", s.vars)
		}
		if len(s.retids) != 0 {
			args = append(args, "ret", s.retids)
		}
		if s.breakDst != nil {
			args = append(args, "break", s.breakDst.ptr())
		}
		if s.continueDst != nil {
			args = append(args, "cont", s.continueDst.ptr())
		}

		tr.Printw("scope", args...)

		for _, id := range s.phi {
			x := s.Exprs[id]

			tr.Printw("phi", "id", id, "tp", s.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}

		for _, id := range s.code {
			x := s.Exprs[id]

			tr.Printw("code", "id", id, "tp", s.EType[id], "typ", tlog.NextAsType, x, "val", x)
		}
	}

	tr.Printw("compiled")

	par.walk(printScope)

	s.exit.walk(func(s *Scope) {
		printScope(s)

		if len(s.phi) != 0 && s.labid == -1 {
			panic(s)
		}

		if s.labid != -1 {
			f.Code = append(f.Code, s.labid)
		}

		for _, id := range s.phi {
			f.Code = append(f.Code, id)
		}

		for _, id := range s.code {
			f.Code = append(f.Code, id)
		}
	}, par)

	return fid, nil
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

		tlog.SpanFromContext(ctx).Printw("return", "ids", ids)

		s.ret(ids)
	case *ast.AssignStmt:
		ids := make([]ir.Expr, len(x.Lhs))

		if len(ids) != len(x.Rhs) {
			panic("bad assignment")
		}

		for i, e := range x.Rhs {
			ids[i], err = c.compileExpr(ctx, s, e)
			if err != nil {
				return nil, errors.Wrap(err, "assignment rhs")
			}
		}

		for i, e := range x.Lhs {
			if v, ok := e.(*ast.Ident); ok {
				if x.Tok == token.DEFINE {
					s.define(v.Name, ids[i])
				} else {
					s.assign(v.Name, ids[i])
				}

				continue
			}

			panic(e)
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
			s.branchFind(-1, func(s *Scope) *Scope { return s.breakDst })
		case token.CONTINUE:
			s.branchFind(-1, func(s *Scope) *Scope { return s.continueDst })
		default:
			panic(x)
		}
	default:
		panic(x)
	}

	return s, nil
}

func (c *Front) compileIf(ctx context.Context, prev *Scope, x *ast.IfStmt) (_ *Scope, err error) {
	tr := tlog.SpanFromContext(ctx)

	var labels []ir.Label

	scond := prev.nextScope(-1, 1, prev)
	next := prev.nextScope(prev.label(), 0)

	if x.Else == nil {
		cond, condExpr, err := c.compileCond(ctx, scond, x.Cond)
		if err != nil {
			return nil, errors.Wrap(err, "if cond")
		}

		scond.branchCond(next, condExpr, revcond(cond))

		//	sub := scond.nextScope(scond.label(), 1, scond)
		//	scond.branchTo(sub)
		sub := scond

		sub, err = c.compileBlock(ctx, sub, x.Body)
		if err != nil {
			return nil, errors.Wrap(err, "then")
		}

		sub.branchTo(next)

		return next, nil
	}

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
		}, prev.untyped)

		switch qq := q.Else.(type) {
		case *ast.BlockStmt:
			lab := prev.label()
			labels = append(labels, lab)

			scond.add(ir.B{
				Label: lab,
			}, prev.untyped)

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

	tr.Printw("if prev", "ptr", prev.ptr(), "lab", prev.lab)
	tr.Printw("if next", "ptr", next.ptr(), "lab", next.lab)

	return next, nil
}

func (c *Front) compileFor(ctx context.Context, prev *Scope, x *ast.ForStmt) (_ *Scope, err error) {
	tr := tlog.SpanFromContext(ctx)

	var scond, sbody, snext *Scope

	if x.Cond != nil {
		scond = prev.nextScope(prev.label(), 1, prev)
		sbody = prev.nextScope(prev.label(), 2, scond)
		snext = prev.nextScope(prev.label(), 0)
	} else {
		scond = prev.nextScope(prev.label(), 1, prev)
		sbody = scond
		snext = prev.nextScope(prev.label(), 0)
	}

	phi := map[definition]ir.Expr{}

	scond.valuef = func(def definition, visited visitSet) (id ir.Expr) {
		if tlog.If("custom_value") {
			defer func() {
				tlog.Printw("custom value", "ptr", scond.ptr(), "d", scond.depth, "def", def, "id", id, "from", loc.Callers(2, 3))
			}()
		}

		id, ok := phi[def]
		if ok {
			return id
		}

		id = prev.findValue(def, visited)
		if id < 0 {
			panic(id)
		}

		typ := prev.EType[id]
		if id < 0 {
			panic(id)
		}

		id = scond.addphi(ir.Phi{}, typ)
		phi[def] = id

		return id
	}

	defer func() {
		scond.valuef = nil

		for def, id := range phi {
			scond.vars[def] = id
			scond.Exprs[id] = scond.mergeValues(def, nil)
			scond.EType[id] = scond.EType[scond.Exprs[id].(ir.Phi)[0].Expr]

			tlog.Printw("fix phi", "ptr", scond.ptr(), "d", scond.depth, "def", def, "id", id, "phi", scond.Exprs[id])
		}
	}()

	sbody.breakDst = snext
	sbody.continueDst = scond

	// cond

	prev.branchTo(scond)

	if x.Cond != nil {
		cond, condExpr, err := c.compileCond(ctx, scond, x.Cond)
		if err != nil {
			return nil, errors.Wrap(err, "cond")
		}

		scond.branchCond(sbody, condExpr, cond)
		scond.branchTo(snext)
	}

	// body

	sbodyEnd, err := c.compileBlock(ctx, sbody, x.Body)
	if err != nil {
		return nil, errors.Wrap(err, "body")
	}

	sbodyEnd.branchTo(scond)

	tr.Printw("for prev", "ptr", prev.ptr(), "lab", prev.lab)
	tr.Printw("for next", "ptr", snext.ptr(), "lab", snext.lab)

	return snext, nil
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
		id = s.value(e.Name)
		if id != -1 {
			return id, nil
		}

		d, ok := s.queue[e.Name]
		if !ok {
			// TODO
			panic(e.Name)
		}

		delete(s.queue, e.Name)

		err = c.compileDecl(ctx, s.root, d)
		if err != nil {
			return -1, errors.Wrap(err, "compile decl %v", e.Name)
		}

		id = s.value(e.Name)
		if id != -1 {
			return id, nil
		}

		// TODO
		panic(e.Name)
		//	return id, errors.New("undefined var: %s", e)
	case *ast.BasicLit:
		switch e.Kind {
		case token.INT:
			x, err := strconv.ParseUint(e.Value, 10, 64)
			if err != nil {
				return -1, errors.Wrap(err, "")
			}

			id = s.add(ir.Imm(x), s.Int)
		default:
			panic(e.Kind)
		}
	case *ast.BinaryExpr:
		l, err := c.compileExpr(ctx, s, e.X)
		if err != nil {
			return -1, errors.Wrap(err, "%v x", e.Op)
		}

		r, err := c.compileExpr(ctx, s, e.Y)
		if err != nil {
			return -1, errors.Wrap(err, "%v y", e.Op)
		}

		var op any
		typ := s.EType[l]

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

			typ = s.cmpt
		default:
			panic(e.Op)
		}

		id = s.add(op, typ)
	case *ast.CallExpr:
		f, err := c.compileExpr(ctx, s, e.Fun)
		if err != nil {
			return f, errors.Wrap(err, "func")
		}

		x := ir.Call{
			Func: f,
			In:   make([]ir.Expr, len(e.Args)),
		}

		for i, a := range e.Args {
			x.In[i], err = c.compileExpr(ctx, s, a)
			if err != nil {
				return -1, errors.Wrap(err, "func call arg")
			}
		}

		ftp := s.EType[f]
		ftyp := s.Exprs[ftp].(*tp.Func)

		id = s.add(x, -1)

		for i, tp := range ftyp.Out {
			if i == 0 {
				s.EType[id] = tp
				continue
			}

			s.add(ir.Out(i), tp)
		}
	default:
		panic(fmt.Sprintf("%T: %[1]v", e))
	}

	return id, nil
}

func (c *Front) compileType(ctx context.Context, s *Scope, e ast.Expr) (id ir.Type, err error) {
	predefined := func(idp *ir.Type, name string) (ir.Type, error) {
		if *idp != 0 {
			return *idp, nil
		}

		switch name {
		case "int":
			*idp = s.addType(tp.Int{Signed: true})
		default:
			panic(name)
		}

		return *idp, nil
	}

	switch e := e.(type) {
	case *ast.Ident:
		switch e.Name {
		case "int":
			return predefined(&s.predefined.Int, e.Name)
		default:
			panic(e.Name)
		}
	default:
		panic(fmt.Sprintf("%T: %[1]v", e))
	}
}

func rootScope(pkg *pkgContext, fun *funContext) *Scope {
	return &Scope{
		pkgContext: pkg,
		funContext: fun,
		lab:        -1,
		labid:      -1,
		defs:       make(map[string]definition),
		vars:       make(map[definition]ir.Expr),
	}
}

func (s *Scope) nextScope(lab ir.Label, delta int, prev ...*Scope) *Scope {
	n := rootScope(s.pkgContext, s.funContext)

	if lab != -1 {
		n.lab = lab
		n.labid = s.alloc(lab, s.untyped)
	}

	n.depth = s.depth + delta
	n.prev = prev

	n.from = loc.Caller(1)

	tlog.V("scope").Printw("new scope", "ptr", n.ptr(), "d", n.depth, "prev", n.prevPtr(), "par", s.ptr(), "from", loc.Callers(1, 3))

	return n
}

func (s *Scope) addphi(x any, tp ir.Type) ir.Expr {
	id := s.pkgContext.alloc(x, tp)
	s.phi = append(s.phi, id)

	return id
}

func (s *Scope) add(x any, tp ir.Type) ir.Expr {
	id := s.pkgContext.alloc(x, tp)
	s.code = append(s.code, id)

	return id
}

func (s *Scope) define(name string, id ir.Expr) {
	if _, ok := s.defs[name]; ok {
		panic(name)
	}

	def := s.nextdef
	s.nextdef++

	tlog.V("vars,define").Printw("define var", "ptr", s.ptr(), "d", s.depth, "def", def, "name", name, "id", id, "from", loc.Callers(1, 3))

	s.defs[name] = def
	s.vars[def] = id
}

func (s *Scope) assign(name string, id ir.Expr) {
	var def definition = -1

	ok := s.find(func(s *Scope) bool {
		tlog.V("value").Printw("find def", "ptr", s.ptr(), "d", s.depth, "defs", s.defs, "ok", func() bool { _, ok := s.defs[name]; return ok }(), "prev", s.prevPtr())

		if x, ok := s.defs[name]; ok {
			def = x
			return true
		}

		return false
	})
	if !ok {
		panic("var not found")
	}

	s.vars[def] = id
}

func (s *Scope) value(name string) (id ir.Expr) {
	var def definition = -1

	if tlog.If("vars,value") {
		defer func() {
			tlog.Printw("var value", "ptr", s.ptr(), "d", s.depth, "def", def, "name", name, "id", id, "from", loc.Callers(1, 3))
		}()
	}

	ok := s.find(func(s *Scope) bool {
		tlog.V("value").Printw("find def", "ptr", s.ptr(), "d", s.depth, "defs", s.defs, "ok", func() bool { _, ok := s.defs[name]; return ok }(), "prev", s.prevPtr())

		if x, ok := s.defs[name]; ok {
			def = x
			return true
		}

		return false
	})
	if !ok {
		return walkNotFound
	}

	return s.findValue(def, nil)
}

func (s *Scope) findValue(def definition, visited visitSet) (id ir.Expr) {
	if !visited.Enter(s) {
		return walkCycled
	}

	if tlog.If("vars,findValue") {
		defer func() {
			tlog.Printw("find value", "ptr", s.ptr(), "d", s.depth, "def", def, "id", id, "from", loc.Callers(1, 4))
		}()
	}

	id, ok := s.vars[def]
	if ok {
		return id
	}

	if f := s.valuef; f != nil {
		return f(def, visited)
	}

	switch len(s.prev) {
	case 0:
		return walkNotFound
	case 1:
		return s.prev[0].findValue(def, visited)
	}

	return s.findValueMerge(def, visited)
}

func (s *Scope) findValueMerge(def definition, visited visitSet) ir.Expr {
	phi := s.mergeValues(def, visited)

	if len(phi) == 1 {
		return phi[0].Expr
	}

	id := s.addphi(phi, s.EType[phi[0].Expr])
	s.vars[def] = id

	return id
}

func (s *Scope) mergeValues(def definition, visited visitSet) ir.Phi {
	var phi ir.Phi

	for _, p := range s.prev {
		b := p.finalBranch()
		id := p.findValue(def, visited)
		if id == walkCycled {
			continue
		}

		phi = append(phi, ir.PhiBranch{
			B:    b,
			Expr: id,
		})
	}

	return phi
}

func (s *Scope) ret(ids []ir.Expr) {
	if len(s.Func.Out) != len(ids) {
		panic(ids)
	}

	// TODO: check types

	s.retids = ids
	s.branchTo(s.exit)
}

func (s *Scope) branchFind(lab ir.Label, f func(s *Scope) *Scope) {
	var dst *Scope

	ok := s.find(func(s *Scope) bool {
		if dst = f(s); dst != nil && (lab == -1 || (s.lab == lab)) {
			return true
		}

		return false
	})
	if !ok {
		panic("break dst not found")
	}

	s.branchTo(dst)
}

func (s *Scope) branchTo(to *Scope) {
	if l := len(s.code); l != 0 {
		id := s.code[l-1]
		if _, ok := s.Exprs[id].(ir.B); ok {
			return
		}
	}

	if to.labid == -1 {
		panic(to)
	}

	id := s.add(ir.B{Label: to.lab}, s.untyped)
	to.appendPrev(s)

	tlog.V("branch").Printw("branch to", "lab", to.lab, "b", id, "dst", to.ptr(), "src", s.ptr(), "from", loc.Callers(1, 3))
}

func (s *Scope) branchCond(to *Scope, expr ir.Expr, cond ir.Cond) {
	if to.labid == -1 {
		panic(to)
	}

	id := s.add(ir.BCond{
		Expr:  expr,
		Cond:  cond,
		Label: to.lab,
	}, s.untyped)

	to.appendPrev(s)

	tlog.V("branch").Printw("branch cond", "lab", to.lab, "b", id, "dst", to.ptr(), "src", s.ptr(), "expr", id, "cond", cond, "from", loc.Callers(1, 3))
}

func (s *Scope) finalBranch() ir.Expr {
	if l := len(s.code); l != 0 {
		id := s.code[l-1]
		if _, ok := s.Exprs[id].(ir.B); ok {
			return id
		}
	}

	return walkNotFound
}

func (s *Scope) appendPrev(prev *Scope) {
	for _, p := range s.prev {
		if p == prev {
			return
		}
	}

	s.prev = append(s.prev, prev)
}

func (s *Scope) ptr() uintptr {
	return uintptr(unsafe.Pointer(s)) & 0xfffff
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

func (s *Scope) find(f func(*Scope) bool) bool {
	if f(s) {
		return true
	}

	if len(s.prev) == 0 {
		return false
	}

	p := s.prev[0]

	for p != nil && p.depth > s.depth {
		p = p.prev[0]
	}

	if p == nil {
		return false
	}

	return p.find(f)
}

func (s *Scope) walk(f func(*Scope), stop ...*Scope) {
	visited := make(visitSet)

	for _, s := range stop {
		visited.Mark(s)
	}

	var walk func(*Scope)
	walk = func(s *Scope) {
		if visited.Marked(s) {
			return
		}

		visited.Mark(s)

		for _, p := range s.prev {
			walk(p)
		}

		f(s)
	}

	walk(s)
}

func (p *pkgContext) id() ir.Expr {
	return ir.Expr(len(p.Exprs))
}

func (p *pkgContext) addType(x any) ir.Type {
	return ir.Type(p.alloc(x, p.typeDef))
}

func (p *pkgContext) alloc(x any, tp ir.Type) ir.Expr {
	id := p.id()
	p.Exprs = append(p.Exprs, x)
	p.EType = append(p.EType, tp)

	return id
}

func (p *pkgContext) label() ir.Label {
	l := p.nextlabel
	p.nextlabel++
	return l
}

func (v visitSet) Mark(s *Scope) {
	v[s] = struct{}{}
}

func (v visitSet) Marked(s *Scope) bool {
	_, ok := v[s]
	return ok
}

func (v *visitSet) Enter(s *Scope) bool {
	if *v == nil {
		*v = make(visitSet)
	}

	if v.Marked(s) {
		return false
	}

	v.Mark(s)

	return true
}

func revcond(x ir.Cond) ir.Cond {
	switch x {
	case "==":
		return "!="
	case "!=":
		return "=="
	case "<":
		return ">="
	case ">":
		return "<="
	case "<=":
		return ">"
	case ">=":
		return "<"
	default:
		panic(x)
	}
}
