package parse

import (
	"context"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Expr struct{}
)

func (p Expr) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	r := LeftToRight{
		Op:  Add{},
		Arg: Int{},
	}

	return r.Parse(ctx, b, st)
}
