package parse

import (
	"bytes"
	"context"
	"unicode/utf8"

	"github.com/nikandfor/errors"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Const []byte

	Ident []byte

	Bool struct{}
)

func (p Const) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	if bytes.HasPrefix(b[st:], p) {
		return Const(b[st : st+len(p)]), st + len(p), nil
	}

	return nil, st, errors.New("%q expected", []byte(p))
}

func (p Ident) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	if st == len(b) {
		return nil, st, errors.New("Ident expected")
	}

	i = st

	c := b[i]

	switch {
	case c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_':
		i++
	default:
		return nil, st, errors.New("Ident expected")
	}

loop:
	for i < len(b) {
		c := b[i]

		switch {
		case c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9' || c == '_':
			i++
		case c >= utf8.RuneSelf:
			if r, w := utf8.DecodeRune(b[i:]); r == utf8.RuneError {
				return nil, i, errors.New("bad rune")
			} else {
				i += w
			}
		default:
			break loop
		}
	}

	return Ident(b[st:i]), i, nil
}

func (Bool) Parse(ctx context.Context, b []byte, st int) (_ ast.Node, i int, err error) {
	if bytes.HasPrefix(b[st:], []byte("true")) {
		return true, st + 4, nil
	}

	if bytes.HasPrefix(b[st:], []byte("false")) {
		return false, st + 5, nil
	}

	return nil, st, errors.New("Bool expected")
}
