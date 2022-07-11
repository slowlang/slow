package parse

import (
	"context"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Type struct {
	}
)

func (p Type) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	x, i, err = Ident{}.Parse(ctx, b, st)
	if err != nil {
		return
	}

	id := x.(ast.Ident)

	x = ast.Type{
		Base: id.Base,
	}

	return
}
