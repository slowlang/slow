package front

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/hacked/hfmt"
	"github.com/nikandfor/loc"
	"github.com/nikandfor/tlog"
)

type (
	State struct {
		b []byte // all files concatenated

		prog Prog

		files []file
	}

	Scope struct {
	}

	Prog struct {
		Package Ident

		Funcs []*Func
	}

	Func struct {
		Name    Ident
		Args    Args
		RetArgs Args
		Body    *Block

		syms map[string]Expr

		obj []byte
	}

	Block struct {
		Stmts []Stmt
	}

	Assignment struct {
		Pos int
		Lhs Expr
		Rhs Expr
	}

	Return struct {
		Pos   int
		Value Expr
	}

	Args []Arg

	Arg struct {
		Num  int
		Name Ident
		Type Ident
	}

	Word struct {
		Value int64
	}

	Sum struct {
		Left  Expr
		Right Expr
	}

	Token interface{}
	Expr  interface{}
	Stmt  interface{}

	Char    byte
	Keyword []byte
	Number  []byte
	Ident   []byte

	file struct {
		Name string
		Base int
	}

	Num struct {
		pos, end int
		float    bool
	}

	UnexpectedError struct {
		Token Token
		Want  []Token
	}
)

func New() *State {
	return &State{}
}

func (s *State) AddFile(ctx context.Context, name string, text []byte) {
	f := file{
		Name: name,
		Base: len(s.b),
	}

	s.b = append(s.b, text...)

	s.files = append(s.files, f)
}

func (s *State) Compile(ctx context.Context) ([]byte, error) {
	b, err := s.compileFunc(ctx, nil, s.prog.Funcs[0])
	if err != nil {
		return nil, err
	}

	return b, nil
}

func (s *State) compileFunc(ctx context.Context, b []byte, f *Func) ([]byte, error) {
	b = hfmt.AppendPrintf(b, `
.align 4
.global _%s
_%[1]s:
	STP	FP, LR, [SP, #-16]!
	MOV	FP, SP

`, f.Name)

	r, ok := f.syms[string(f.RetArgs[0].Name)]
	if !ok {
		return nil, errors.New("no expr for ret arg")
	}

	b, err := s.compileExpr(ctx, b, f, 0, r)
	if err != nil {
		return nil, err
	}

	b = append(b, `
	LDP	FP, LR, [SP], #16
	RET
`...)

	return b, nil
}

func (s *State) compileExpr(ctx context.Context, b []byte, f *Func, reg int, e Expr) (_ []byte, err error) {
	switch e := e.(type) {
	case Word:
		b = s.compileConst(ctx, b, reg, e)
	case Arg:
		if reg == e.Num {
			return b, nil
		}

		b = hfmt.AppendPrintf(b, "	MOV	X%d, X%d\n", reg, e.Num)
	case Sum:
		b, err = s.compileExpr(ctx, b, f, reg+1, e.Left)
		if err != nil {
			return nil, errors.Wrap(err, "left")
		}

		if y, ok := e.Right.(Word); ok && y.Value == 0 {
			// nothing to do
		} else if y.Value > 0 && y.Value < 1<<12 {
			b = hfmt.AppendPrintf(b, "	ADD	X%d, X%d, #%d\n", reg, reg+1, y.Value)
		} else {
			b, err = s.compileExpr(ctx, b, f, reg+2, e.Right)
			if err != nil {
				return nil, errors.Wrap(err, "right")
			}

			b = hfmt.AppendPrintf(b, "	ADD	X%d, X%d, X%d\n", reg, reg+1, reg+2)
		}
	default:
		panic(e)
	}

	return b, nil
}

func (s *State) compileConst(ctx context.Context, b []byte, reg int, y Word) (_ []byte) {
	b = hfmt.AppendPrintf(b, "	MOV	X%v, #%d\n", reg, y.Value&0xffff)

	for sh := 0; y.Value > 0xffff; sh += 16 {
		y.Value >>= 16

		b = hfmt.AppendPrintf(b, "	MOV	X%v, #%d, LSL #%d // Word\n", reg, y.Value&0xffff, sh)
	}

	return b
}

func (s *State) Analyze(ctx context.Context) error {
	err := s.analyzeFunc(ctx, s.prog.Funcs[0])
	if err != nil {
		return err
	}

	return nil
}

func (s *State) analyzeFunc(ctx context.Context, f *Func) error {
	f.syms = make(map[string]Expr)

	for _, a := range f.Args {
		f.syms[string(a.Name)] = a
	}

	err := s.analyzeBlock(ctx, f, f.Body)
	if err != nil {
		return err
	}

	ret, ok := f.syms[string(f.RetArgs[0].Name)]
	if !ok {
		return errors.New("no result")
	}

	tlog.SpanFromContext(ctx).Printw("func", "name", f.Name, "result", ret)
	tlog.SpanFromContext(ctx).Printw("func", "syms", f.syms)

	return nil
}

func (s *State) analyzeBlock(ctx context.Context, f *Func, b *Block) (err error) {
	for _, st := range b.Stmts {
		var pos int

		switch st := st.(type) {
		case Assignment:
			pos = st.Pos
			err = s.analyzeAssignment(ctx, f, st)
		case Return:
			pos = st.Pos
			err = s.analyzeReturn(ctx, f, st)
		}

		if err != nil {
			return errors.Wrap(err, "at pos 0x%x", pos)
		}
	}

	return nil
}

func (s *State) analyzeReturn(ctx context.Context, f *Func, r Return) error {
	e, err := s.analyzeRhsExpr(ctx, f, r.Value)
	if err != nil {
		return err
	}

	tlog.SpanFromContext(ctx).Printw("return", "val", r.Value, "expr", e)

	f.syms[string(f.RetArgs[0].Name)] = e

	return nil
}

func (s *State) analyzeAssignment(ctx context.Context, f *Func, a Assignment) error {
	name, err := s.analyzeLhsExpr(ctx, f, a.Lhs)
	if err != nil {
		return err
	}

	e, err := s.analyzeRhsExpr(ctx, f, a.Rhs)
	if err != nil {
		return err
	}

	f.syms[string(name.(Ident))] = e

	tlog.SpanFromContext(ctx).Printw("assign", "lhs", a.Lhs, "rhs", a.Rhs, "expr", e)

	return nil
}

func (s *State) analyzeLhsExpr(ctx context.Context, f *Func, e Expr) (Expr, error) {
	switch e := e.(type) {
	case Ident:
		return e, nil
	case Number:
		return nil, errors.New("rhs expected, got %v", e)
	default:
		panic(e)
	}
}

func (s *State) analyzeRhsExpr(ctx context.Context, f *Func, e Expr) (Expr, error) {
	switch e := e.(type) {
	case Word:
		return e, nil
	case Number:
		v, err := strconv.ParseInt(string(e), 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "parse int")
		}

		return Word{Value: v}, nil
	case Ident:
		r, ok := f.syms[string(e)]
		if !ok {
			return nil, errors.New("unknown symbol: %q", e)
		}

		return s.analyzeRhsExpr(ctx, f, r)
	case Arg:
		return e, nil
	case Sum:
		l, err := s.analyzeRhsExpr(ctx, f, e.Left)
		if err != nil {
			return nil, errors.Wrap(err, "analyze left")
		}

		r, err := s.analyzeRhsExpr(ctx, f, e.Right)
		if err != nil {
			return nil, errors.Wrap(err, "analyze right")
		}

		return Sum{Left: l, Right: r}, nil
	default:
		panic(e)
	}
}

func (s *State) Parse(ctx context.Context) error {
	f, i, err := s.parseFunc(ctx, 0)
	if err != nil {
		return errors.Wrap(err, "at pos 0x%x", i)
	}

	s.prog.Funcs = append(s.prog.Funcs, f)

	tlog.SpanFromContext(ctx).Printw("func", "f", f)

	return nil
}

func (s *State) parseFunc(ctx context.Context, st int) (f *Func, i int, err error) {
	tk, tst, i := s.next(ctx, st)
	if kw, ok := tk.(Keyword); !ok || string(kw) != "func" {
		return nil, tst, NewUnexpected(tk, Keyword("func"))
	}

	tk, tst, i = s.next(ctx, i)
	name, ok := tk.(Ident)
	if !ok {
		return nil, tst, NewUnexpected(tk, Ident{})
	}

	args, i, err := s.parseArgs(ctx, i)
	if err != nil {
		return
	}

	var ret Args
	tk, tst, _ = s.next(ctx, i)
	if c, ok := tk.(Char); ok && c == Char('(') {
		ret, i, err = s.parseArgs(ctx, i)
		if err != nil {
			return
		}
	}

	b, i, err := s.parseBlock(ctx, i)
	if err != nil {
		return
	}

	f = &Func{
		Name:    name,
		Args:    args,
		RetArgs: ret,
		Body:    b,
	}

	return
}

func (s *State) parseArgs(ctx context.Context, st int) (a Args, i int, err error) {
	tk, tst, i := s.next(ctx, st)
	if tk != Char('(') {
		return nil, tst, NewUnexpected(tk, Char('('))
	}

loop:
	for {
		j := i
		tk, tst, i = s.next(ctx, i)
		switch tk {
		case Char('\n'), Char(','):
			continue
		case Char(')'):
			break loop
		default:
			i = j
		}

		tk, tst, i = s.next(ctx, i)
		name, ok := tk.(Ident)
		if !ok {
			return nil, tst, NewUnexpected(tk, Ident{})
		}

		tk, tst, i = s.next(ctx, i)
		typ, ok := tk.(Ident)
		if !ok {
			return nil, tst, NewUnexpected(tk, Ident{})
		}

		a = append(a, Arg{
			Name: name,
			Type: typ,
		})
	}

	return
}

func (s *State) parseBlock(ctx context.Context, st int) (b *Block, i int, err error) {
	tk, tst, i := s.next(ctx, st)
	if tk != Char('{') {
		return nil, tst, NewUnexpected(tk, Char('{'))
	}

	b = &Block{}

loop:
	for {
		j := i
		tk, tst, i = s.next(ctx, i)
		switch tk {
		case Char('\n'), Char(';'):
			continue
		case Char('}'):
			break loop
		default:
			i = j
		}

		var stmt Stmt
		stmt, i, err = s.parseStatement(ctx, i)
		if err != nil {
			return
		}

		b.Stmts = append(b.Stmts, stmt)
	}

	return
}

func (s *State) parseStatement(ctx context.Context, st int) (x Stmt, i int, err error) {
	tk, tst, i := s.next(ctx, st)

	switch tk := tk.(type) {
	case Ident:
		return s.parseAssignment(ctx, st)
	case Keyword:
		switch string(tk) {
		case "return":
			return s.parseReturn(ctx, st, i)
		default:
			return nil, tst, NewUnexpected(tk, Keyword{})
		}
	default:
		return nil, tst, NewUnexpected(tk, Ident{}, Keyword{})
	}
}

func (s *State) parseReturn(ctx context.Context, st, vst int) (x Stmt, i int, err error) {
	exp, i, err := s.parseExpr(ctx, vst)
	if err != nil {
		return nil, i, errors.Wrap(err, "return")
	}

	tlog.SpanFromContext(ctx).Printw("return", "val", exp)

	return Return{Pos: st, Value: exp}, i, nil
}

func (s *State) parseAssignment(ctx context.Context, st int) (x Stmt, i int, err error) {
	lhs, i, err := s.parseExpr(ctx, st)
	if err != nil {
		return x, i, errors.Wrap(err, "lhs")
	}

	tk, tst, i := s.next(ctx, i)
	if tk != Char('=') {
		return x, tst, NewUnexpected(tk, Char('='))
	}

	rhs, i, err := s.parseExpr(ctx, i)
	if err != nil {
		return x, i, errors.Wrap(err, "rhs")
	}

	tlog.SpanFromContext(ctx).Printw("assignment", "lhs", lhs, "rhs", rhs)

	return Assignment{
		Pos: st,
		Lhs: lhs,
		Rhs: rhs,
	}, i, nil
}

func (s *State) parseExpr(ctx context.Context, st int) (_ Expr, i int, err error) {
	return s.parseSum(ctx, st)
}

func (s *State) parseSum(ctx context.Context, st int) (_ Expr, i int, err error) {
	larg, i, err := s.parseExprArg(ctx, st)
	if err != nil {
		return
	}

	for {
		tk, tst, e := s.next(ctx, i)
		if c, ok := tk.(Char); !ok || c != Char('+') {
			i = tst
			break
		}

		var rarg Expr
		rarg, i, err = s.parseExprArg(ctx, e)
		if err != nil {
			return
		}

		larg = Sum{
			Left:  larg,
			Right: rarg,
		}
	}

	return larg, i, nil
}

func (s *State) parseExprArg(ctx context.Context, st int) (_ Expr, i int, err error) {
	tk, tst, i := s.next(ctx, st)

	switch tk := tk.(type) {
	case Number:
		return tk, i, nil
	case Ident:
		return tk, i, nil
	default:
		return nil, tst, NewUnexpected(tk, Number{}, Ident{})
	}
}

func (s *State) next(ctx context.Context, st int) (tk Token, tst int, i int) {
	if tr := tlog.SpanFromContext(ctx); tr.If("next_token") {
		defer func(st int) {
			tr.Printw("next token", "st", st, "tk", tk, "tst", tst, "i", i, "from", loc.Callers(1, 3))
		}(st)
	}

	st = skipSpaces(s.b, st)
	i = st

	if i == len(s.b) {
		return nil, st, i
	}

	c := s.b[i]

	switch c {
	case '[', ']', '{', '}', '(', ')', '=', '+', '-', ';', '\n', '\t':
		return Char(s.b[i]), st, i + 1
	}

	switch {
	case c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z':
		e := skipIdent(s.b, i)

		switch string(s.b[i:e]) {
		case "return", "func", "for", "break", "continue", "import", "var", "const", "type":
			return Keyword(s.b[i:e]), st, e
		}

		return Ident(s.b[i:e]), st, e
	case c >= '0' && c <= '9':
		e := skipNum(s.b, i)
		return Number(s.b[i:e]), st, e
	default:
		panic(c)
	}
}

func (s *State) parseNum(ctx context.Context, st int) (x Num, i int, err error) {
	b := s.b
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
	for i < len(b) && b[i] == ' ' || b[i] == '\t' {
		i++
	}

	return i
}

func (c Char) String() string {
	return string(c)
}
