package parse

import (
	"context"
	"fmt"
	"strings"

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

func (p AllOf) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	i = st

	res := make([]ast.Node, len(p))

	for j, r := range p {
		x, i, err = r.Parse(ctx, b, i)
		if err != nil {
			return nil, i, errors.Wrap(err, "%T (%d)", r, j)
		}

		res[j] = x
	}

	return res, i, nil
}

func (p AnyOf) Parse(ctx context.Context, b []byte, st int) (_ ast.Node, i int, err error) {
	for _, r := range p {
		x, j, e := r.Parse(ctx, b, st)
		if e == nil {
			return x, j, nil
		}
		if j == st {
			continue
		}
		if err == nil {
			i = j
			err = errors.Wrap(e, "%T", r)
		}
	}

	if err != nil {
		return
	}

	return nil, st, errors.New("expected %v", joinHuman(p...))
}

func joinHuman(l ...Parser) string {
	switch len(l) {
	case 0:
		return "<none>"
	case 1:
		return fmt.Sprintf("%T", l[0])
	}

	var b strings.Builder

	for i, r := range l {
		if i+1 == len(l) {
			b.WriteString(" or ")
		} else if i != 0 {
			b.WriteString(", ")
		}

		fmt.Fprintf(&b, "%T", r)
	}

	return b.String()
}
