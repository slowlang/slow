package parse

import (
	"context"

	"github.com/nikandfor/errors"
	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Spaces uint64

	Spacer struct {
		Spaces Spaces
		Of     Parser
	}
)

var (
	Space    = NewSpaces(' ')
	SpaceTab = NewSpaces(' ', '\t')
	SpaceAll = NewSpaces(' ', '\t', '\r', '\n')
)

func NewSpaces(skip ...byte) (ss Spaces) {
	for _, q := range skip {
		if q >= 64 {
			panic("too high char code")
		}

		ss |= 1 << q
	}

	return
}

func (s Spaces) Skip(b []byte, st int) (i int) {
	i = st

	for i < len(b) && b[i] < 64 && s&(1<<b[i]) != 0 {
		i++
	}

	return
}

func Spaced(p Parser, ss Spaces) Spacer {
	return Spacer{
		Spaces: ss,
		Of:     p,
	}
}

func SpacedBy(p Parser, skip ...byte) Spacer {
	return Spacer{
		Spaces: NewSpaces(skip...),
		Of:     p,
	}
}

func (p Spacer) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	vst := p.Spaces.Skip(b, st)

	x, i, err = p.Of.Parse(ctx, b, vst)
	if err != nil {
		if i == vst {
			i = st
		}

		err = errors.Wrap(err, "%T", p.Of)
	}

	return
}
