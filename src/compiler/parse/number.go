package parse

import (
	"context"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Int struct {
	}
)

func (p *Int) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	if st == len(b) {
	}

	i = st

	for i < len(b) && b[i] >= '0' && b[i] <= '9' {
		i++
	}

	if i == st {
		return nil, st, NewTypeExpectedError(p)
	}

	x = ast.Int{
		Base: ast.Base{
			Pos: st,
		},
		End: i,
	}

	return x, i, nil
}
