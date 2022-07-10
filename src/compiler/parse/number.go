package parse

import (
	"context"

	"github.com/nikandfor/errors"
	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	Num struct{}

	Int struct{}

	Float struct{}
)

func (p Num) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	i = st

	if i < len(b) && b[i] == '0' {
		i++

		switch b[i] {
		case 'x', 'X':
		case 'o', 'O':
		case 'n', 'N':
		case 'b', 'B':
		default:
			i -= 2 // don't skip base prefix
		}

		i++ // skip base prefix
	}

	dst := i
	dot := false
	exp := false

loop:
	for ; i < len(b); i++ {
		switch {
		case b[i] >= '0' && b[i] <= '9':
		case !dot && b[i] == '.':
			dot = true
		case !exp && (b[i] == 'e' || b[i] == 'E'):
			exp = true
		default:
			break loop
		}
	}

	if i == dst || i == dst+1 && b[dst] == '.' {
		return nil, st, errors.New("Num expected")
	}

	base := ast.Base{
		Pos: st,
		End: i,
	}
	if dot {
		x = ast.Float{
			Base: base,
		}
	} else {
		x = ast.Int{
			Base: base,
		}
	}

	return
}

func (p Int) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	i = st

	if i < len(b) && b[i] == '0' {
		i++

		switch b[i] {
		case 'x', 'X':
		case 'o', 'O':
		case 'n', 'N':
		case 'b', 'B':
		default:
			i -= 2 // don't skip base prefix
		}

		i++ // skip base prefix
	}

	dst := i

	for i < len(b) && b[i] >= '0' && b[i] <= '9' {
		i++
	}

	if i == dst {
		return nil, st, errors.New("Int expected")
	}

	return ast.Int{
		Base: ast.Base{
			Pos: st,
			End: i,
		},
	}, i, nil
}

func (p Float) Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error) {
	x, i, err = Num{}.Parse(ctx, b, st)
	if err != nil {
		return nil, st, errors.New("Float expected")
	}

	if y, ok := x.(ast.Int); ok {
		x = ast.Float(y)
	}

	return
}
