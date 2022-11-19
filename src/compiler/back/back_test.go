package back

import (
	"context"
	"testing"

	"github.com/slowlang/slow/src/compiler/ir"
)

func TestSmoke(t *testing.T) {
	pkg := &ir.Package{
		Path: "main",
		Funcs: []*ir.Func{
			{
				Name: "main",
			},
		},
	}

	ctx := context.Background()

	var c Compiler

	obj, err := c.CompilePackage(ctx, nil, nil, pkg)
	if err != nil {
		t.Errorf("compile func: %v", err)
	}

	t.Logf("result:\n%s", obj)
}
