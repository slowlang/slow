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

	"github.com/slowlang/slow/src/compiler/ir"
	"github.com/slowlang/slow/src/compiler/tp"
)

type (
	pkgContext struct {
		*ir.Package

		root *Scope

		unty ir.Type
		zero ir.Expr
		self ir.Expr

		def definition
		lab ir.Label

		tasks map[string]any
		types map[string]ir.Type // cache
	}

	funContext struct {
		*ir.Func

		exit *Scope
	}

	Scope struct {
		*funContext
		*pkgContext

		labid ir.Expr
		lab   ir.Label

		from  loc.PC
		depth int
		prev  []*Scope

		cont *Scope
		brea *Scope

		defs map[string]definition
		vars map[definition]ir.Expr

		state ir.State

		phi  []ir.Expr
		code []ir.Expr

		retvals []ir.Expr
		valuef  func(definition, visitSet) ir.Expr
		statef  func(visitSet) ir.State
	}

	definition int

	visitSet map[*Scope]struct{}
)

const (
	walkNotFound = -1 - iota
	walkCycled
)

func (c *Front) Compile(ctx context.Context) (_ *ir.Package, err error) {
	p := &pkgContext{
		Package: &ir.Package{},
		tasks:   map[string]any{},
		types:   map[string]ir.Type{},
	}

	s := rootScope(p, nil)
	s.from = loc.Caller(0)
	p.root = s

	p.unty = s.addType(tp.Untyped{}, -1)
	p.zero = s.alloc(ir.Zero{}, p.unty)
	p.self = s.alloc(p.Package, p.unty)

	for _, f := range c.files {
		for _, d := range f.Decls {
			err = c.addTasks(ctx, s, d)
			if err != nil {
				return nil, errors.Wrap(err, "decl %T", d)
			}
		}
	}

	for name, d := range p.tasks {
		err = c.compileTask(ctx, s, d)
		if err != nil {
			return nil, errors.Wrap(err, "decl %T", d)
		}

		delete(p.tasks, name)
	}

	return p.Package, nil
}

func (c *Front) addTasks(ctx context.Context, s *Scope, d ast.Decl) (err error) {
	switch d := d.(type) {
	case *ast.FuncDecl:
		c.addTask(ctx, s, d.Name.Name, d)
	case *ast.GenDecl:
		for _, spec := range d.Specs {
			switch d := spec.(type) {
			case *ast.TypeSpec:
				c.addTask(ctx, s, d.Name.Name, d)
			default:
				panic(d)
			}
		}
	default:
		panic(d)
	}

	return nil
}

func (c *Front) addTask(ctx context.Context, s *Scope, name string, task any) {
	t, ok := s.tasks[name]
	if ok {
		panic(t)
	}

	s.tasks[name] = task
}

func (c *Front) findFunc(ctx context.Context, s *Scope, name string) (ir.Expr, *ir.Func) {
	for _, id := range s.pkgContext.Funcs {
		f := s.pkgContext.Exprs[id].(*ir.Func)

		if f.Name == name {
			return id, f
		}
	}

	d, ok := s.tasks[name]
	if !ok {
		panic(name)
	}

	fd, ok := d.(*ast.FuncDecl)
	if !ok {
		panic(name)
	}

	id, err := c.compileFunc(ctx, s.root, fd)
	if err != nil {
		panic(err.Error())
	}

	f := s.Exprs[id].(*ir.Func)

	return id, f
}

func (c *Front) compileTask(ctx context.Context, s *Scope, d any) (err error) {
	switch d := d.(type) {
	case *ast.FuncDecl:
		_, err = c.compileFunc(ctx, s, d)
		return err
	case *ast.TypeSpec:
		return c.compileTypeSpec(ctx, s, d)
	default:
		panic(d)
	}
}

//

func (c *Front) compileFunc(ctx context.Context, par *Scope, fn *ast.FuncDecl) (fid ir.Expr, err error) {
	f := &ir.Func{
		Name: fn.Name.Name,
	}
	tp := &tp.Func{}

	s := par.nextScope(-1, 1, par)
	s.funContext = &funContext{
		Func: f,
	}

	ftp := par.addType(tp, -1)
	fid = par.alloc(f, ftp)
	s.Funcs = append(s.Funcs, fid)

	for _, p := range fn.Type.Params.List {
		ptp, err := c.compileType(ctx, par, p.Type)
		if err != nil {
			return -1, errors.Wrap(err, "param type")
		}

		if len(p.Names) == 0 {
			id := s.add(ir.Out(len(f.In)), ptp)
			f.In = append(f.In, id)
			tp.In = append(tp.In, ir.Type(ptp))

			continue
		}

		for _, name := range p.Names {
			id := s.add(ir.Out(len(f.In)), ptp)
			f.In = append(f.In, id)
			tp.In = append(tp.In, ir.Type(ptp))

			s.define(name.Name, id)
		}
	}

	f.StateIn = ir.State(s.add(ir.State(len(f.In)), -1))
	s.state = f.StateIn

	if len(f.In) != 0 {
		s.Exprs[f.In[0]] = ir.Args(len(f.In))
	}

	if fn.Type.Results != nil {
		for _, p := range fn.Type.Results.List {
			ptp, err := c.compileType(ctx, par, p.Type)
			if err != nil {
				return -1, errors.Wrap(err, "param type")
			}

			if len(p.Names) == 0 {
				f.Out = append(f.Out, -1)
				tp.Out = append(tp.Out, ir.Type(ptp))
				continue
			}

			for _, name := range p.Names {
				f.Out = append(f.Out, -1)
				tp.Out = append(tp.Out, ir.Type(ptp))

				s.define(name.Name, s.zero)
			}
		}
	}

	s.Exprs[ftp] = tp
	s.Exprs[fid] = f

	tlog.Printw("compile func", "name", f.Name, "expr", fid, "typ", ftp)

	s.exit = s.nextScope(s.label(), 0)

	end, err := c.compileBlock(ctx, s, fn.Body)
	if err != nil {
		return fid, errors.Wrap(err, "body")
	}

	end.branchTo(s.exit)

	for i := range f.Out {
		var phi ir.Phi

		for _, p := range s.exit.prev {
			phi = append(phi, p.retvals[i])
		}

		f.Out[i] = s.exit.addphi(phi)
	}

	f.StateOut = s.exit.findState(nil)

	tlog.Printw("func scopes", "name", f.Name, "in", f.In, "out", f.Out)

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
		if len(s.retvals) != 0 {
			args = append(args, "ret", s.retvals)
		}

		tlog.Printw("scope", args...)

		for _, id := range s.phi {
			x := s.Exprs[id]
			tp := s.EType[id]
			tlog.Printw("phi", "id", id, "tp", tp, "typ", tlog.NextIsType, x, "val", x)
		}

		for _, id := range s.code {
			x := s.Exprs[id]
			tp := s.EType[id]

			tlog.Printw("code", "id", id, "tp", tp, "typ", tlog.NextIsType, x, "val", x)
		}
	}

	par.walk(printScope)

	s.exit.walk(func(s *Scope) {
		printScope(s)

		if s.lab != -1 {
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
			ids[i], err = c.compileExpr(ctx, s, e, false)
			if err != nil {
				return nil, errors.Wrap(err, "return value")
			}
		}

		s.ret(ids)

		return s, nil
	case *ast.AssignStmt:
		if len(x.Lhs) != len(x.Rhs) {
			panic("bad assign")
		}

		ids := make([]ir.Expr, len(x.Rhs))

		for i, e := range x.Rhs {
			ids[i], err = c.compileExpr(ctx, s, e, false)
			if err != nil {
				return nil, errors.Wrap(err, "assignment rhs")
			}
		}

		for i, e := range x.Lhs {
			tlog.V("assignment").Printw("assign", "lhs", tlog.NextIsType, x.Lhs[i], "rhs", tlog.NextIsType, x.Rhs[i], "id", ids[i])

			if v, ok := e.(*ast.Ident); ok {
				if x.Tok == token.DEFINE {
					s.define(v.Name, ids[i])
				} else {
					s.assign(v.Name, ids[i])
				}

				continue
			}

			ptr, err := c.compileExpr(ctx, s, e, true)
			if err != nil {
				return nil, errors.Wrap(err, "lexpr")
			}

			ss := s.findState(nil)

			id := s.add(ir.Store{
				Ptr:   ptr,
				Val:   ids[i],
				State: ss,
			}, -1)

			s.state = ir.State(id)
		}
	case *ast.IncDecStmt:
		id, err := c.compileExpr(ctx, s, x.X, false)
		if err != nil {
			return s, errors.Wrap(err, "inc expr")
		}

		one := s.add(ir.Imm(1), s.unty)

		var op any

		if x.Tok == token.INC {
			op = ir.Add{L: id, R: one}
		} else {
			op = ir.Sub{L: id, R: one}
		}

		id = s.add(op, s.EType[id])

		if v, ok := x.X.(*ast.Ident); ok {
			s.assign(v.Name, id)
			break
		}

		ptr, err := c.compileExpr(ctx, s, x.X, true)
		if err != nil {
			return nil, errors.Wrap(err, "lexpr")
		}

		ss := s.findState(nil)

		id = s.add(ir.Store{
			Ptr:   ptr,
			Val:   id,
			State: ss,
		}, -1)

		s.state = ir.State(id)
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
			s.doBreak(-1)
		case token.CONTINUE:
			s.doContinue(-1)
		default:
			panic(x)
		}
	case *ast.ExprStmt:
		tlog.Printw("expr stmt", "pos", x.Pos(), "val", x)

		id, err := c.compileExpr(ctx, s, x.X, false)
		if err != nil {
			return nil, errors.Wrap(err, "expr stmt")
		}

		_ = id
	default:
		panic(x)
	}

	return s, nil
}

func (c *Front) compileIf(ctx context.Context, prev *Scope, x *ast.IfStmt) (_ *Scope, err error) {
	var labels []ir.Label

	scond := prev.nextScope(-1, 1, prev)
	next := prev.nextScope(prev.label(), 0)

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
		}, -1)

		switch qq := q.Else.(type) {
		case *ast.BlockStmt:
			lab := prev.label()
			labels = append(labels, lab)

			scond.add(ir.B{
				Label: lab,
			}, -1)

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
	var scond, sbody, snext *Scope

	if x.Cond != nil {
		scond = prev.nextScope(prev.label(), 1, prev)
		sbody = prev.nextScope(prev.label(), 2, scond)
		snext = prev.nextScope(prev.label(), 0, scond)
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

		id = scond.addphi(ir.Phi{})
		phi[def] = id

		return id
	}

	defer func() {
		scond.valuef = nil

		for def, id := range phi {
			scond.vars[def] = id
			scond.Exprs[id] = scond.mergeValues(def, nil)

			tlog.Printw("fix phi", "ptr", scond.ptr(), "d", scond.depth, "def", def, "id", id, "phi", scond.Exprs[id])
		}
	}()

	stateOut := ir.State(-1)
	scond.statef = func(visited visitSet) (id ir.State) {
		if tlog.If("custom_value") {
			defer func() {
				tlog.Printw("custom state", "ptr", scond.ptr(), "d", scond.depth, "def", tlog.None, "id", id, "from", loc.Callers(2, 3))
			}()
		}

		if stateOut != -1 {
			return stateOut
		}

		id = ir.State(scond.addphi(ir.Phi{}))
		stateOut = id

		return id
	}

	defer func() {
		scond.statef = nil

		if stateOut == -1 {
			return
		}

		scond.Exprs[stateOut] = scond.mergeStates(nil)
		scond.state = stateOut
	}()

	sbody.cont = scond
	sbody.brea = snext

	// cond

	prev.branchTo(scond)

	if x.Cond != nil {
		cond, condExpr, err := c.compileCond(ctx, scond, x.Cond)
		if err != nil {
			return nil, errors.Wrap(err, "cond")
		}

		scond.add(ir.BCond{
			Expr:  condExpr,
			Cond:  revcond(cond),
			Label: snext.lab,
		}, -1)

		scond.branchTo(sbody)
	}

	// body

	sbodyEnd, err := c.compileBlock(ctx, sbody, x.Body)
	if err != nil {
		return nil, errors.Wrap(err, "body")
	}

	sbodyEnd.branchTo(scond)

	tlog.Printw("for prev", "p", prev.ptr())
	tlog.Printw("for next", "p", snext.ptr())

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

	id, err = c.compileExpr(ctx, s, e, false)

	return
}

func (c *Front) compileExpr(ctx context.Context, s *Scope, e ast.Expr, lvalue bool) (id ir.Expr, err error) {
	switch e := e.(type) {
	case *ast.Ident:
		id = s.value(e.Name)
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

			id = s.add(ir.Imm(x), s.unty)
		case token.STRING:
			panic(e.Value)

			data, err := strconv.Unquote(e.Value)
			if err != nil {
				panic(err)
			}

			_ = data

		//	id = s.add(ir.Data(data + "\000")) // TODO
		//	id = s.add(ir.Ptr{X: id})
		//	id = s.add(ir.Struct{id, s.add(ir.Imm(len(data)))})
		default:
			panic(e.Kind)
		}
	case *ast.UnaryExpr:
		id, err = c.compileExpr(ctx, s, e.X, e.Op == token.AND)
		if err != nil {
			return -1, errors.Wrap(err, "operand")
		}

		switch e.Op {
		case token.AND:
		//	id = s.add(ir.Ptr{X: id})
		default:
			panic(e.Op)
		}
	case *ast.BinaryExpr:
		l, err := c.compileExpr(ctx, s, e.X, false)
		if err != nil {
			return -1, errors.Wrap(err, "op lhs")
		}

		r, err := c.compileExpr(ctx, s, e.Y, false)
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

		id = s.add(op, s.EType[l])
	case *ast.IndexExpr:
		var base ir.Expr

		base, err = c.compileExpr(ctx, s, e.X, true)
		if err != nil {
			return 0, errors.Wrap(err, "index base")
		}

		idx, err := c.compileExpr(ctx, s, e.Index, false)
		if err != nil {
			return 0, errors.Wrap(err, "index idx")
		}

		id = s.add(ir.Offset{
			Base:   base,
			Offset: idx,
			Size:   s.add(ir.Imm(8), s.unty),
		}, s.EType[base])

		if lvalue {
			break
		}

		ss := s.findState(nil)

		id = s.add(ir.Load{Ptr: id, State: ss}, -1)
	case *ast.CallExpr:
		n := e.Fun.(*ast.Ident)

		fid, fdef := c.findFunc(ctx, s, n.Name)

		x := ir.Call{
			Func: fid,
			Args: make([]ir.Expr, len(e.Args)),
		}

		for i, a := range e.Args {
			x.Args[i], err = c.compileExpr(ctx, s, a, false)
			if err != nil {
				return -1, errors.Wrap(err, "op lhs")
			}
		}

		id = s.add(x, -1)

		for i := 1; i < len(fdef.Out); i++ {
			s.add(ir.Out(i), -1)
		}
	case *ast.CompositeLit:
		tp, err := c.compileType(ctx, s, e.Type)
		if err != nil {
			return 0, errors.Wrap(err, "type")
		}

		id = s.add(ir.Alloc{Type: tp}, tp)
	default:
		panic(fmt.Sprintf("%T: %[1]v", e))
	}

	return
}

func (c *Front) compileTypeSpec(ctx context.Context, s *Scope, spec *ast.TypeSpec) (err error) {
	id, err := c.compileType(ctx, s, spec.Type)
	if err != nil {
		return errors.Wrap(err, "type expr")
	}

	tlog.Printw("type", "name", spec.Name.Name, "id", id)

	s.types[spec.Name.Name] = id

	return nil
}

func (c *Front) compileType(ctx context.Context, s *Scope, e ast.Expr) (id ir.Type, err error) {
	switch e := e.(type) {
	case *ast.Ident:
		if id, ok := s.types[e.Name]; ok {
			return id, nil
		}

		switch e.Name {
		case "int":
			id = s.addType(tp.Int{Bits: 64, Signed: true}, -1)
		default:
			panic(e.Name)
		}

		s.types[e.Name] = id
	case *ast.ArrayType:
		id, err = c.compileType(ctx, s, e.Elt)
		if err != nil {
			return id, errors.Wrap(err, "arr elem")
		}

		l, err := c.compileExpr(ctx, s, e.Len, false)
		if err != nil {
			return id, errors.Wrap(err, "arr len")
		}

		id = s.addType(tp.Array{Elem: id, Len: l}, -1)
	case *ast.StarExpr:
		id, err = c.compileType(ctx, s, e.X)
		if err != nil {
			return id, errors.Wrap(err, "")
		}

		x := tp.Ptr{X: s.Exprs[id].(tp.Type)}
		id = s.addType(x, -1)
	case *ast.StructType:
		x := tp.Struct{}

		for _, f := range e.Fields.List {
			ft, err := c.compileType(ctx, s, f.Type)
			if err != nil {
				return -1, errors.Wrap(err, "field type: %v", f.Type)
			}

			if len(f.Names) == 0 {
				x.Fields = append(x.Fields, tp.StructField{
					Name:   "",
					Offset: 0,
					Type:   s.Exprs[ft].(tp.Type),
				})
			}

			for _, n := range f.Names {
				x.Fields = append(x.Fields, tp.StructField{
					Name:   n.Name,
					Offset: 0,
					Type:   s.Exprs[ft].(tp.Type),
				})
			}
		}

		id = s.addType(x, -1)
	default:
		panic(fmt.Sprintf("%T: %[1]v", e))
	}

	return id, nil
}

func rootScope(pkg *pkgContext, fun *funContext) *Scope {
	return &Scope{
		pkgContext: pkg,
		funContext: fun,
		labid:      -1,
		lab:        -1,
		vars:       make(map[definition]ir.Expr),
		state:      -1,
	}
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

func (s *Scope) nextScope(lab ir.Label, delta int, prev ...*Scope) *Scope {
	next := rootScope(s.pkgContext, s.funContext)

	if lab != -1 {
		next.lab = lab
		next.labid = s.alloc(lab, -1)
	}

	next.from = loc.Caller(1)
	next.depth = s.depth + delta
	next.prev = prev

	return next
}

func (s *Scope) addphi(x any) ir.Expr {
	id := s.pkgContext.alloc(x, -1)
	s.phi = append(s.phi, id)

	return id
}

func (s *Scope) add(x any, tp ir.Type) ir.Expr {
	id := s.pkgContext.alloc(x, tp)
	s.code = append(s.code, id)

	return id
}

func (s *Scope) addType(x any, tp ir.Type) ir.Type {
	return ir.Type(s.add(x, tp))
}

/*
func (s *Scope) addTyped(x any, tp ir.Type) ir.Expr {
	id := s.pkgContext.alloc(x, tp)
	s.code = append(s.code, id)

	return id
}
*/

func (s *Scope) define(name string, id ir.Expr) {
	_, ok := s.findDef(name, s.depth)
	if ok {
		panic(name)
	}

	if s.defs == nil {
		s.defs = map[string]definition{}
	}

	tlog.V("vars").Printw("define var", "ptr", s.ptr(), "def", s.def, "name", name, "id", id, "from", loc.Callers(1, 3))

	s.defs[name] = s.def
	s.vars[s.def] = id
	s.def++
}

func (s *Scope) assign(name string, id ir.Expr) {
	def, ok := s.findDef(name, s.depth)
	if !ok {
		panic(name)
	}

	tlog.V("vars").Printw("assign var", "ptr", s.ptr(), "def", def, "name", name, "id", id, "from", loc.Callers(1, 3))

	s.vars[def] = id
}

func (s *Scope) value(name string) (id ir.Expr) {
	def, ok := s.findDef(name, s.depth)
	if !ok {
		panic(name)
	}

	if tlog.If("vars") {
		defer func() {
			tlog.Printw("get var", "ptr", s.ptr(), "def", def, "name", name, "id", id, "from", loc.Callers(1, 3))
		}()
	}

	return s.findValue(def, nil)
}

func (s *Scope) findValue(def definition, visited visitSet) (id ir.Expr) {
	if !visited.Enter(s) {
		return walkCycled
	}

	if tlog.If("findValue") {
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
		return phi[0]
	}

	id := s.addphi(phi)
	s.vars[def] = id

	return id
}

func (s *Scope) mergeValues(def definition, visited visitSet) ir.Phi {
	var phi ir.Phi

	for _, p := range s.prev {
		id := p.findValue(def, visited)
		if id == walkCycled {
			continue
		}

		phi = append(phi, id)
	}

	return phi
}

func (s *Scope) findState(visited visitSet) (id ir.State) {
	if !visited.Enter(s) {
		return walkCycled
	}

	if s.state >= 0 {
		return s.state
	}

	if f := s.statef; f != nil {
		return f(visited)
	}

	switch len(s.prev) {
	case 0:
		return walkNotFound
	case 1:
		return s.prev[0].findState(visited)
	}

	return s.findStateMerge(visited)
}

func (s *Scope) findStateMerge(visited visitSet) ir.State {
	phi := s.mergeStates(visited)
	id := s.addphi(phi)

	s.state = ir.State(id)

	return ir.State(id)
}

func (s *Scope) mergeStates(visited visitSet) ir.Phi {
	var phi ir.Phi

	for _, p := range s.prev {
		id := p.findState(visited)

		phi = append(phi, ir.Expr(id))
	}

	return phi
}

func (s *Scope) findDef(name string, depth int) (def definition, ok bool) {
	ok = s.find(func(s *Scope) bool {
		def, ok = s.defs[name]
		return ok
	}, s.depth)

	return
}

func (s *Scope) ret(ids []ir.Expr) {
	if len(s.Func.Out) != len(ids) {
		panic(ids)
	}

	// TODO: check types

	s.retvals = ids
	s.branchTo(s.exit)
}

func (s *Scope) doContinue(lab ir.Expr) {
	var head *Scope

	ok := s.find(func(s *Scope) bool {
		if lab != -1 && s.labid != lab {
			return false
		} else if s.cont == nil {
			return false
		}

		head = s

		return true
	}, s.depth)
	if !ok {
		panic("not ok")
	}
	if head.cont == nil {
		panic("where?")
	}

	s.branchTo(head.cont)
}

func (s *Scope) doBreak(lab ir.Expr) {
	var head *Scope

	ok := s.find(func(s *Scope) bool {
		if lab != -1 && s.labid != lab {
			return false
		} else if s.cont == nil {
			return false
		}

		head = s

		return true
	}, s.depth)
	if !ok {
		panic("not ok")
	}
	if head.brea == nil {
		panic("where?")
	}

	s.branchTo(head.brea)
}

func (s *Scope) branchTo(to *Scope) {
	if l := len(s.code); l != 0 {
		if _, ok := s.Exprs[s.code[l-1]].(ir.B); ok {
			return
		}
	}

	if to.labid == -1 {
		panic(to)
	}

	s.add(ir.B{Label: to.lab}, -1)
	to.appendPrev(s)
}

func (s *Scope) appendPrev(prev *Scope) {
	for _, p := range s.prev {
		if p == prev {
			return
		}
	}

	s.prev = append(s.prev, prev)
}

func (s *Scope) find(f func(*Scope) bool, depth int) bool {
	if s.depth <= depth {
		if f(s) {
			return true
		}
	}

	if len(s.prev) == 0 {
		return false
	}

	return s.prev[0].find(f, s.depth)
}

func (s *Scope) walk(f func(s *Scope), stop ...*Scope) {
	done := make(visitSet)

	for _, s := range stop {
		done.Mark(s)
	}

	var walk func(*Scope)
	walk = func(s *Scope) {
		if _, ok := done[s]; ok {
			return
		}

		done[s] = struct{}{}

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

func (p *pkgContext) alloc(x any, tp ir.Type) ir.Expr {
	id := p.id()
	p.Exprs = append(p.Exprs, x)
	p.EType = append(p.EType, tp)

	return id
}

func (p *pkgContext) typeid() ir.Type {
	return ir.Type(p.id())
}

//func (p *pkgContext) addType(x any) ir.Type {
//	return ir.Type(p.alloc(x, -1))
//}

func (p *pkgContext) label() ir.Label {
	l := p.lab
	p.lab++
	return l
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
