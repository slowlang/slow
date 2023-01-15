package front

import (
	"context"
	"go/ast"
	"go/parser"
	"go/token"
)

type (
	Front struct {
		fset *token.FileSet

		files []*ast.File
	}
)

func New() *Front {
	return &Front{
		fset: token.NewFileSet(),
	}
}

func (c *Front) AddFile(ctx context.Context, name string, text []byte) error {
	f, err := parser.ParseFile(c.fset, name, text, 0)
	if err != nil {
		return err
	}

	c.files = append(c.files, f)

	return nil
}

func (c *Front) Parse(ctx context.Context) error { return nil }
