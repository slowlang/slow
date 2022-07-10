package parse

import (
	"context"

	"github.com/nikandfor/errors"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Math struct {
		of Parser
	}

	LeftToRight struct {
		Op  Parser
		Arg Parser
	}

	BinOper interface {
		BinOp(l, r ast.Node) (ast.Node, error)
	}

	Add struct{}
)

func (p LeftToRight) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	x, i, err = p.Arg.Parse(ctx, b, st)
	if err != nil {
		return nil, i, errors.Wrap(err, "first arg")
	}

	for i < len(b) {
		var op ast.Node
		opst := i
		op, i, err = p.Op.Parse(ctx, b, i)
		if i == opst {
			err = nil
			break
		}
		if err != nil {
			return nil, i, errors.Wrap(err, "op")
		}

		c, ok := op.(BinOper)
		if !ok {
			return nil, i, errors.New("BinOper expected, got %T", op)
		}

		var r ast.Node
		r, i, err = p.Arg.Parse(ctx, b, i)
		if err != nil {
			return nil, i, errors.Wrap(err, "arg")
		}

		x, err = c.BinOp(x, r)
		if err != nil {
			return nil, i, errors.Wrap(err, "%T", c)
		}
	}

	return
}

func (p Add) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	x, i, err = Const("+").Parse(ctx, b, st)
	if err != nil {
		return
	}

	return Add{}, i, nil
}

func (p Add) BinOp(l, r ast.Node) (ast.Node, error) {
	return ast.Add{
		Left:  l,
		Right: r,
	}, nil
}
