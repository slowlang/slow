package parse

import (
	"bytes"
	"context"

	"github.com/nikandfor/errors"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Const []byte
)

func (p Const) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	if bytes.HasPrefix(b[st:], p) {
		return Const(b[st : st+len(p)]), st + len(p), nil
	}

	return nil, st, errors.New("%q expected", []byte(p))
}
