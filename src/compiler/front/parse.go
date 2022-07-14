package front

import (
	"bytes"
	"context"
	"fmt"
	"strings"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"
	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Front struct {
		b   []byte // all files concatenated
		lim int    // end of current file

		files []*File
	}

	Word struct {
		Value int64
	}

	Arch interface {
		AllocReg(hint int) int
	}

	ARM64A struct {
	}

	Token interface{}
	Expr  interface{}
	Stmt  interface{}

	Char    byte
	Keyword string

	File struct {
		name string
		base int

		parsed *ast.File
	}

	Num struct {
		pos, end int
		float    bool
	}

	UnexpectedError struct {
		Token Token
		Want  []Token
	}

	eoftp struct{}
)

var eof eoftp

func New() *Front {
	return &Front{}
}

func (fc *Front) AddFile(ctx context.Context, name string, text []byte) *File {
	f := &File{
		name: name,
		base: len(fc.b),
	}

	fc.b = append(fc.b, text...)

	fc.files = append(fc.files, f)

	return f
}

func (fc *Front) Parse(ctx context.Context) (err error) {
	for j := range fc.files {
		ff, i, err := fc.parseFile(ctx, j)
		if err != nil {
			file, line, col := fc.getPos(i)

			return errors.Wrap(err, "%v:%v:%v", file, line, col)
		}

		fc.files[j].parsed = ff
	}

	return nil
}

func (fc *Front) parseFile(ctx context.Context, j int) (ff *ast.File, i int, err error) {
	i = fc.files[j].base
	fc.lim = len(fc.b)
	if j+1 < len(fc.files) {
		fc.lim = fc.files[j+1].base
	}

	ff = &ast.File{}

loop:
	for {
		tk, tst, e := fc.next(ctx, i)
		switch tk {
		case eof:
			break loop
		case Char('\n'):
			i = e
			continue
		case Keyword("func"):
			var f *ast.Func

			f, i, err = fc.parseFunc(ctx, i)
			if err != nil {
				return nil, i, errors.Wrap(err, "func")
			}

			ff.Funcs = append(ff.Funcs, f)

			tlog.SpanFromContext(ctx).Printw("func", "f", f)
		default:
			return nil, tst, NewUnexpected(tk, Keyword("func"))
		}
	}

	return ff, i, nil
}

func (fc *Front) parseFunc(ctx context.Context, st int) (f *ast.Func, i int, err error) {
	tk, tst, i := fc.next(ctx, st)
	if kw, ok := tk.(Keyword); !ok || string(kw) != "func" {
		return nil, tst, NewUnexpected(tk, Keyword("func"))
	}

	tk, tst, i = fc.next(ctx, i)
	name, ok := tk.(ast.Ident)
	if !ok {
		return nil, tst, NewUnexpected(tk, ast.Ident(""))
	}

	args, i, err := fc.parseArgs(ctx, i)
	if err != nil {
		return nil, i, errors.Wrap(err, "args")
	}

	var ret ast.Args
	tk, tst, _ = fc.next(ctx, i)
	if c, ok := tk.(Char); ok && c == Char('(') {
		ret, i, err = fc.parseArgs(ctx, i)
		if err != nil {
			return nil, i, errors.Wrap(err, "ret args")
		}
	}

	b, i, err := fc.parseBlock(ctx, i)
	if err != nil {
		return nil, i, errors.Wrap(err, "body")
	}

	f = &ast.Func{
		Name:    string(name),
		Args:    args,
		RetArgs: ret,
		Body:    b,
	}

	return
}

func (fc *Front) parseArgs(ctx context.Context, st int) (a ast.Args, i int, err error) {
	tk, tst, i := fc.next(ctx, st)
	if tk != Char('(') {
		return nil, tst, NewUnexpected(tk, Char('('))
	}

loop:
	for {
		j := i
		tk, tst, i = fc.next(ctx, i)
		switch tk {
		case Char('\n'), Char(','):
			continue
		case Char(')'):
			break loop
		default:
			i = j
		}

		tk, tst, i = fc.next(ctx, i)
		name, ok := tk.(ast.Ident)
		if !ok {
			return nil, tst, NewUnexpected(tk, ast.Ident(""))
		}

		tk, tst, i = fc.next(ctx, i)
		typ, ok := tk.(ast.Ident)
		if !ok {
			return nil, tst, NewUnexpected(tk, ast.Ident(""))
		}

		a = append(a, ast.Arg{
			Name: string(name),
			Type: string(typ),
		})
	}

	return
}

func (fc *Front) parseBlock(ctx context.Context, st int) (b *ast.Block, i int, err error) {
	tk, tst, i := fc.next(ctx, st)
	if tk != Char('{') {
		return nil, tst, NewUnexpected(tk, Char('{'))
	}

	b = &ast.Block{}

loop:
	for {
		j := i
		tk, tst, i = fc.next(ctx, i)
		switch tk {
		case Char('\n'), Char(';'):
			continue
		case Char('}'):
			break loop
		default:
			i = j
		}

		var stmt Stmt
		stmt, i, err = fc.parseStatement(ctx, i)
		if err != nil {
			return
		}

		b.Stmts = append(b.Stmts, stmt)
	}

	return
}

func (fc *Front) parseStatement(ctx context.Context, st int) (x ast.Stmt, i int, err error) {
	tk, tst, i := fc.next(ctx, st)

	switch tk := tk.(type) {
	case ast.Ident:
		return fc.parseAssignment(ctx, st)
	case Keyword:
		switch string(tk) {
		case "return":
			return fc.parseReturn(ctx, st, i)
		case "if":
			return fc.parseIf(ctx, st, i)
		default:
			return nil, tst, NewUnexpected(tk, Keyword(""))
		}
	default:
		return nil, tst, NewUnexpected(tk, ast.Ident(""), Keyword(""))
	}
}

func (fc *Front) parseReturn(ctx context.Context, st, vst int) (x ast.Stmt, i int, err error) {
	exp, i, err := fc.parseExpr(ctx, vst)
	if err != nil {
		return nil, i, errors.Wrap(err, "return")
	}

	tlog.SpanFromContext(ctx).Printw("return", "val", exp)

	return ast.Return{Pos: st, Value: exp}, i, nil
}

func (fc *Front) parseIf(ctx context.Context, st, vst int) (x ast.Stmt, i int, err error) {
	exp, i, err := fc.parseExpr(ctx, vst)
	if err != nil {
		return nil, i, errors.Wrap(err, "condition")
	}

	b, i, err := fc.parseBlock(ctx, i)
	if err != nil {
		return nil, i, errors.Wrap(err, "then block")
	}

	tlog.SpanFromContext(ctx).Printw("if stmt", "cond", exp, "then", b)

	return &ast.IfStmt{Pos: st, Cond: exp, Then: b}, i, nil
}

func (fc *Front) parseAssignment(ctx context.Context, st int) (x ast.Stmt, i int, err error) {
	lhs, i, err := fc.parseExpr(ctx, st)
	if err != nil {
		return x, i, errors.Wrap(err, "lhs")
	}

	tk, tst, i := fc.next(ctx, i)
	if tk != ast.Op("=") {
		return x, tst, NewUnexpected(tk, ast.Op("="))
	}

	rhs, i, err := fc.parseExpr(ctx, i)
	if err != nil {
		return x, i, errors.Wrap(err, "rhs")
	}

	tlog.SpanFromContext(ctx).Printw("assignment", "lhs", lhs, "rhs", rhs)

	return ast.Assignment{
		Pos: st,
		Lhs: lhs,
		Rhs: rhs,
	}, i, nil
}

func (fc *Front) parseExpr(ctx context.Context, st int) (x ast.Expr, i int, err error) {
	return fc.parseCmp(ctx, st)
}

func (fc *Front) parseCmp(ctx context.Context, st int) (x ast.Expr, i int, err error) {
	x, i, err = fc.parseSum(ctx, st)
	if err != nil {
		return nil, i, errors.Wrap(err, "sum")
	}

	tk, tst, e := fc.next(ctx, i)
	op, ok := tk.(ast.Op)
	if !ok {
		return x, tst, nil
	}

	switch op {
	case "==", "<", ">", "<=", ">=":
	default:
		return x, tst, nil
	}

	r, i, err := fc.parseSum(ctx, e)
	if err != nil {
		return nil, i, errors.Wrap(err, "sum")
	}

	x = ast.BinOp{
		Op:    op,
		Left:  x,
		Right: r,
	}

	return x, i, nil
}

func (fc *Front) parseSum(ctx context.Context, st int) (x ast.Expr, i int, err error) {
	larg, i, err := fc.parseMul(ctx, st)
	if err != nil {
		return nil, i, errors.Wrap(err, "mul")
	}

	for {
		tk, tst, e := fc.next(ctx, i)
		op, ok := tk.(ast.Op)
		if !ok || op != "+" && op != "-" {
			i = tst
			break
		}

		var rarg Expr
		rarg, i, err = fc.parseMul(ctx, e)
		if err != nil {
			return nil, i, errors.Wrap(err, "mul")
		}

		larg = ast.BinOp{
			Op:    op,
			Left:  larg,
			Right: rarg,
		}
	}

	return larg, i, nil
}

func (fc *Front) parseMul(ctx context.Context, st int) (_ ast.Expr, i int, err error) {
	larg, i, err := fc.parseExprArg(ctx, st)
	if err != nil {
		return nil, i, errors.Wrap(err, "arg")
	}

	for {
		tk, tst, e := fc.next(ctx, i)
		op, ok := tk.(ast.Op)
		if !ok || op != "*" && op != "/" {
			i = tst
			break
		}

		var rarg Expr
		rarg, i, err = fc.parseExprArg(ctx, e)
		if err != nil {
			return nil, i, errors.Wrap(err, "arg")
		}

		larg = ast.BinOp{
			Op:    op,
			Left:  larg,
			Right: rarg,
		}
	}

	return larg, i, nil
}

func (fc *Front) parseExprArg(ctx context.Context, st int) (_ ast.Expr, i int, err error) {
	tk, tst, i := fc.next(ctx, st)

	switch tk := tk.(type) {
	case ast.Number:
		return tk, i, nil
	case ast.Ident:
		return tk, i, nil
	default:
		return nil, tst, NewUnexpected(tk, ast.Number(""), ast.Ident(""))
	}
}

func (fc *Front) next(ctx context.Context, st int) (tk Token, tst int, i int) {
	if tr := tlog.SpanFromContext(ctx); tr.If("next_token") {
		defer func(st int) {
			tr.Printw("next token", "st", st, "tk", tk, "tst", tst, "i", i, "from", loc.Callers(1, 3))
		}(st)
	}

	i = st

again:
	st = skipSpaces(fc.b, i)
	i = st

	if i >= fc.lim {
		return eof, st, i
	}

	c := fc.b[i]

	switch c {
	case '[', ']', '{', '}', '(', ')',
		';', '\n', '\t':

		return Char(fc.b[i]), st, i + 1
	case '=', '+', '-', '>', '<', '&', '|':
		if i+1 < fc.lim && (fc.b[i+1] == c || fc.b[i+1] == '=') {
			return ast.Op(fc.b[i : i+2]), st, i + 2
		}

		return ast.Op(fc.b[i : i+1]), st, i + 1
	}

	switch {
	case c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z':
		e := skipIdent(fc.b, i)

		switch string(fc.b[i:e]) {
		case "return", "func", "for", "break", "continue", "import", "var", "const", "type", "if", "else":
			return Keyword(fc.b[i:e]), st, e
		}

		return ast.Ident(fc.b[i:e]), st, e
	case c >= '0' && c <= '9':
		e := skipNum(fc.b, i)
		return ast.Number(fc.b[i:e]), st, e
	case bytes.HasPrefix(fc.b[i:], []byte("//")):
		e := nextLine(fc.b, i)
		// ignore comments
		i = e

		goto again
	case bytes.HasPrefix(fc.b[i:], []byte("/*")):
		e := bytes.Index(fc.b[i:], []byte("*/"))
		if e == -1 {
			panic("no end of comment")
		}

		i += e + 2

		goto again
	default:
		file, line, col := fc.getPos(st)

		panic(fmt.Sprintf("%v:%d:%d  unexpected %q", file, line, col, c))
	}
}

func (fc *Front) parseNum(ctx context.Context, st int) (x Num, i int, err error) {
	b := fc.b
	i = st

	if i < len(b) && b[i] == '0' {
		i++

		switch b[i] {
		case 'x', 'X':
		case 'o', 'O':
		case 'n', 'N':
		case 'b', 'B':
		default:
			i -= 2 // don't skip base prefix
		}

		i++ // skip base prefix
	}

	dst := i
	dot := false
	exp := false

loop:
	for ; i < len(b); i++ {
		switch {
		case b[i] >= '0' && b[i] <= '9':
		case !dot && b[i] == '.':
			dot = true
		case !exp && (b[i] == 'e' || b[i] == 'E'):
			exp = true
		default:
			break loop
		}
	}

	if i == dst || i == dst+1 && b[dst] == '.' {
		return x, st, errors.New("Num expected")
	}

	x = Num{
		pos:   st,
		end:   i,
		float: dot,
	}

	return
}

func (fc *Front) getPos(p int) (file string, line, col int) {
	for i, f := range fc.files {
		if p < f.base {
			continue
		}

		file = f.name

		lim := len(fc.b)
		if i+1 < len(fc.files) {
			lim = fc.files[i+1].base
		}

		for lst := f.base; lst < lim; {
			line++

			next := nextLine(fc.b, lst)
			if next < p {
				lst = next
				continue
			}

			col = 1 + p - lst

			return
		}
	}

	panic(p)
}

func NewUnexpected(got Token, want ...Token) error {
	return UnexpectedError{
		Token: got,
		Want:  want,
	}
}

func (e UnexpectedError) Error() string {
	l := make([]string, len(e.Want))

	for i := range e.Want {
		l[i] = fmt.Sprintf("%T", e.Want[i])
	}

	return fmt.Sprintf("unexpected token: %q (%[1]T) want: %v", e.Token, strings.Join(l, ", "))
}

func nextLine(b []byte, i int) int {
	for i < len(b) && b[i] != '\n' {
		i++
	}

	return i + 1
}

func skipNum(b []byte, i int) int {
	for i < len(b) && (b[i] >= '0' && b[i] <= '9') {
		i++
	}

	return i
}

func skipIdent(b []byte, i int) int {
	for i < len(b) && (b[i] >= 'a' && b[i] <= 'z' || b[i] >= 'A' && b[i] <= 'Z' || b[i] >= '0' && b[i] <= '9' || b[i] == '_') {
		i++
	}

	return i
}

func skipSpaces(b []byte, i int) int {
	for i < len(b) && (b[i] == ' ' || b[i] == '\t') {
		i++
	}

	return i
}

func (c Char) String() string {
	return string(c)
}

func (f *File) Parsed() *ast.File {
	return f.parsed
}
