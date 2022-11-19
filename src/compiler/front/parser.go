package front

import (
	"context"
	"os"

	"github.com/nikandfor/errors"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Parser struct{}

	token   any
	punct   string
	ident   string
	comment string
)

func (p *Parser) ParseFile(ctx context.Context, name string) (*ast.File, error) {
	data, err := os.ReadFile(name)
	if err != nil {
		return nil, errors.Wrap(err, "read")
	}

	return p.ParseFileData(ctx, data)
}

func (p *Parser) ParseFileData(ctx context.Context, b []byte) (f *ast.File, err error) {
	f = &ast.File{}

	for i := 0; i < len(b); {
		var fn *ast.Func

		fn, i, err = p.parseFunc(ctx, b, i)
		if err != nil {
			return nil, err
		}

		f.Funcs = append(f.Funcs, fn)
	}

	return f, nil
}

func (p *Parser) parseFunc(ctx context.Context, b []byte, st int) (fn *ast.Func, i int, err error) {
	var t token

	i = st

	for {
		t, i, err = p.token(ctx, b, i)
		if err != nil {
			return
		}

		if _, ok := t.(comment); ok {
			continue
		}

		if t != ident("func") {
			return nil, st, errors.New("func expected")
		}

		break
	}
}

func (p *Parser) token(ctx context.Context, b []byte, st int) (t token, i int, err error) {
	st = skipSpaces(b, st)
	i = st

	if i == len(b) {
		return nil, i, nil
	}

	switch c := b[i]; c {
	case '(', ')', '{', '}':
		return punct(b[i : i+1]), i + 1, nil
	case '/':
		if i+1 == len(b) {
			return nil, i, nil
		}

		c2 := b[i+1]
		switch {
		case c == '/' && c2 == '/':
			i = skipLine(b, i)

			return comment(b[st:i]), i, nil
		case c == c2:
			return punct(b[i : i+2]), i + 2, nil
		}

		return punct(b[i : i+1]), i + 1, nil
	default:
		if c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c == '_' {
			i = skipIdent(b, i+1)

			return ident(b[st:i]), i, nil
		}

		return nil, i, errors.New("unsupported token: %q", c)
	}

	return
}

func skipSpaces(b []byte, i int) int {
	for i < len(b) {
		switch b[i] {
		case ' ', '\t', '\n':
			i++
			continue
		}

		break
	}

	return i
}

func skipIdent(b []byte, i int) int {
	for i < len(b) && (b[i] == '_' ||
		b[i] >= 'A' && b[i] <= 'Z' ||
		b[i] >= 'a' && b[i] <= 'z' ||
		b[i] >= '0' && b[i] <= '9') {
		i++
	}

	return i
}

func skipLine(b []byte, i int) int {
	for i < len(b) && b[i] != '\n' {
		i++
	}

	return i
}
