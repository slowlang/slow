package parse

import (
	"context"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	VarDecl struct{}

	Assignment struct{}
)

func (p VarDecl) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	r := AllOf{
		Const("var"),
		Spaced(Ident{}, SpaceTab),
		Spaced(Type{}, SpaceTab),
	}

	x, i, err = r.Parse(ctx, b, st)
	if err != nil {
		return
	}

	xt := x.([]ast.Node)

	res := ast.VarDecl{
		Base: ast.Base{
			Pos: st,
			End: i,
		},
		Name: xt[1].(ast.Ident),
		Type: xt[2].(ast.Type),
	}

	return res, i, nil
}

func (p Assignment) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	r := AllOf{
		Ident{},
		Spaced(Const("var"), SpaceTab),
		Spaced(Expr{}, SpaceTab),
	}

	x, i, err = r.Parse(ctx, b, st)
	if err != nil {
		return
	}

	xt := x.([]ast.Node)

	res := ast.Assignment{
		Base: ast.Base{
			Pos: st,
			End: i,
		},
		Left:  xt[0].(ast.Ident),
		Right: xt[2],
	}

	return res, i, nil
}
