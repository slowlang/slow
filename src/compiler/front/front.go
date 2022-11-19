//go:build ignore

package front

import (
	"context"
	"os"

	"github.com/nikandfor/errors"
	"github.com/slowlang/slow/src/compiler/ir"
)

type (
	Compiler struct {
	}

	state struct {
		c *Compiler
		p *ir.Package

		b []byte
	}

	token interface{}

	ident   []byte
	punct   []byte
	comment []byte

	param struct {
		Name ident
		Type ident
	}
)

func New() *Compiler { return nil }

func (c *Compiler) CompileFile(ctx context.Context, name string) (*ir.Package, error) {
	data, err := os.ReadFile(name)
	if err != nil {
		return nil, errors.Wrap(err, "read")
	}

	s := &state{
		c: c,
		p: new(ir.Package),
		b: data,
	}

	err = c.parse(ctx, s)
	if err != nil {
		return nil, errors.Wrap(err, "parse")
	}

	return s.p, nil
}

func (c *Compiler) parse(ctx context.Context, s *state) error {
	for i := 0; i < len(s.b); {
		t, e, err := s.token(i)
		if err != nil {
			err = errors.Wrap(err, "read token")
			return errors.Wrap(err, "at pos %d", i)
		}

		switch t := t.(type) {
		case nil:
			i = e
		case ident:
			switch string(t) {
			case "func":
				i, err = c.parseFunc(ctx, s, e)
			default:
				err = errors.New("unexpected ident: %s", t)
			}
		default:
			err = errors.New("unsupported token: %s (%[1]T)", t)
		}

		if err != nil {
			return errors.Wrap(err, "at pos %d", i)
		}
	}

	return nil
}

func (c *Compiler) parseFunc(ctx context.Context, s *state, st int) (i int, err error) {
	t, i, err := s.token(st)
	if err != nil {
		return i, err
	}

	name, ok := t.(ident)
	if !ok {
		return st, errors.New("expected func name, got %T", t)
	}

	f := &ir.Func{
		Name: string(name),
	}

	params, i, err := c.parseParams(ctx, s, i)
	if err != nil {
		return
	}

	f.In = make([]ir.Param, len(params))

	params, i, err = c.parseParams(ctx, s, i)
	if err != nil {
		return
	}

	f.Out = make([]ir.Param, len(params))

	i, err = c.parseBlock(ctx, s, i, f)
	if err != nil {
		return
	}

	s.p.Funcs = append(s.p.Funcs, f)

	return
}

func (c *Compiler) parseParams(ctx context.Context, s *state, st int) (p []param, i int, err error) {
	t, i, err := s.token(st)
	if err != nil {
		return
	}

	switch t := t.(type) {
	case punct:
		if string(t) != "(" {
			return nil, st, errors.New("unexpected token: %q", t)
		}
	case ident:
		p = []param{
			{Type: t},
		}

		return
	default:
		return nil, st, errors.New("unexpected token: %T", t)
	}

	for {
		t, i, err = s.token(i)
		if err != nil {
			return
		}

		if p, ok := t.(punct); ok && string(p) == ")" {
			break
		}

		name, ok := t.(ident)
		if !ok {
			err = errors.New("param name expected, got %v (%[1]T)", t)
			return
		}

		t, i, err = s.token(i)
		if err != nil {
			return
		}

		typ, ok := t.(ident)
		if !ok {
			err = errors.New("param type expected, got %v (%[1]T)", t)
			return
		}

		p = append(p, param{
			Name: name,
			Type: typ,
		})
	}

	return
}

func (c *Compiler) parseBlock(ctx context.Context, s *state, st int, f *ir.Func) (i int, err error) {
	t, i, err := s.token(st)
	if err != nil {
		return
	}

	if p, ok := t.(punct); !ok || string(p) != "{" {
		return st, errors.New("open bracket expected, got %s (%[1]T)", t)
	}

	for {
		t, _, err = s.token(i)
		if err != nil {
			return
		}

		if p, ok := t.(punct); ok && string(p) == "}" {
			break
		}

		if _, ok := t.(comment); ok {
			continue
		}

		i, err = c.parseStmt(ctx, s, i, f)
		if err != nil {
			return
		}
	}

	return
}

func (c *Compiler) parseStmt(ctx context.Context, s *state, st int, f *ir.Func) (i int, err error) {
	t, i, err = s.token(st)
	if err != nil {
		return
	}

	id, ok := t.(ident)
	if !ok {
		return i, errors.New("token: %s %[1]T", t)
	}

	switch string(id) {
	//	case "return":
	//	case "if":
	//	case "for":
	}

	expr, i, err := c.parseExpr(ctx, s, st, f)
	if err != nil {
		return
	}
}

func (s *state) token(st int) (t token, i int, err error) {
	st = s.skipSpaces(st)
	i = st

	if i == len(s.b) {
		return nil, i, nil
	}

	switch c := s.b[i]; c {
	case '(', ')', '{', '}':
		return punct(s.b[i : i+1]), i + 1, nil
	case '/':
		if i+1 == len(s.b) {
			return nil, i, nil
		}

		c2 := s.b[i+1]
		switch {
		case c == '/' && c2 == '/':
			i = s.skipLine(i)

			return comment(s.b[st:i]), i, nil
		case c == c2:
			return punct(s.b[i : i+2]), i + 2, nil
		}

		return punct(s.b[i : i+1]), i + 1, nil
	default:
		if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_' {
			i = s.skipIdent(i + 1)

			return ident(s.b[st:i]), i, nil
		}

		return nil, i, errors.New("unsupported token: %q", c)
	}

	return
}

func (s *state) skipSpaces(i int) int {
	for i < len(s.b) {
		switch s.b[i] {
		case ' ', '\t', '\n':
			i++
			continue
		}

		break
	}

	return i
}

func (s *state) skipIdent(i int) int {
	for i < len(s.b) && (s.b[i] == '_' ||
		s.b[i] >= 'A' && s.b[i] <= 'Z' ||
		s.b[i] >= 'a' && s.b[i] <= 'z' ||
		s.b[i] >= '0' && s.b[i] <= '9') {
		i++
	}

	return i
}

func (s *state) skipLine(i int) int {
	for i < len(s.b) && s.b[i] != '\n' {
		i++
	}

	return i
}
