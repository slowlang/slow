package parse

import (
	"context"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	FuncDecl struct {
	}
)

func (p FuncDecl) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	return
}
