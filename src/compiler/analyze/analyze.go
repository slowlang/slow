package analyze

import (
	"context"
	"fmt"
	"reflect"
	"strconv"

	"github.com/nikandfor/errors"

	"github.com/slowlang/slow/src/compiler/ast"
	"github.com/slowlang/slow/src/compiler/ir"
	"github.com/slowlang/slow/src/compiler/parse"
)

type (
	UnsupportedASTNodeError struct{ T ast.Node }
)

func Analyze(ctx context.Context, st *parse.State, x ast.Node) (y ir.Node, err error) {
	switch x := x.(type) {
	case ast.Int:
		v, err := strconv.ParseUint(string(st.Text(x.Pos, x.End)), 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "parse Int value")
		}

		w := ir.Word{
			Value: v,
		}

		return w, nil
	default:
		return nil, NewUnsupportedASTNode(x)
	}
}

func NewUnsupportedASTNode(x ast.Node) UnsupportedASTNodeError {
	return UnsupportedASTNodeError{
		T: x,
	}
}

func (e UnsupportedASTNodeError) Error() string {
	return fmt.Sprintf("unsupported node: %v", reflect.TypeOf(e.T))
}
