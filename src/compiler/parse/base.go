package parse

import (
	"context"

	"github.com/nikandfor/errors"
	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	None struct{}

	Optional struct {
		Parser
	}

	Context struct {
		Pre  Parser
		Of   Parser
		Post Parser
	}

	AllOf []Parser

	AnyOf []Parser
)

func (None) Parse(ctx context.Context, b []byte, st int) (_ ast.Node, i int, err error) {
	return None{}, st, nil
}

func (p Optional) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	x, i, err = p.Parser.Parse(ctx, b, st)
	if i == st {
		return None{}, st, nil
	}

	return
}

func (p Context) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	i = st

	if p.Pre != nil {
		_, i, err = p.Pre.Parse(ctx, b, i)
		if err != nil {
			return nil, i, errors.Wrap(err, "pre")
		}
	}

	vst := i

	x, i, err = p.Of.Parse(ctx, b, i)
	if err != nil {
		if i == vst {
			i = st
		}

		return nil, i, errors.Wrap(err, "of")
	}

	if p.Post != nil {
		_, i, err = p.Post.Parse(ctx, b, i)
		if err != nil {
			return nil, i, errors.Wrap(err, "post")
		}
	}

	return x, i, nil
}
