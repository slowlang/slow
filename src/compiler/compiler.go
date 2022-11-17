package compiler

import (
	"context"
	"os"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"

	"github.com/slowlang/slow/src/compiler/ast"
	"github.com/slowlang/slow/src/compiler/back"
	"github.com/slowlang/slow/src/compiler/front"
)

func ParseFile(ctx context.Context, name string) (_ *ast.File, err error) {
	text, err := os.ReadFile(name)
	if err != nil {
		return nil, errors.Wrap(err, "read file")
	}

	tlog.SpanFromContext(ctx).Printw("read file", "size", len(text), "name", name)

	return Parse(ctx, name, text)
}

func Parse(ctx context.Context, name string, text []byte) (_ *ast.File, err error) {
	st := front.New()

	f := st.AddFile(ctx, name, text)

	err = st.Parse(ctx)
	if err != nil {
		//	tlog.SpanFromContext(ctx).Printw("abstract syntax tree", "x_type", tlog.FormatNext("%T"), x, "x", x, "err", err)
		return nil, errors.Wrap(err, "parse text")
	}

	return f.Parsed(), nil
}

func CompileFile(ctx context.Context, name string) (obj []byte, err error) {
	text, err := os.ReadFile(name)
	if err != nil {
		return nil, errors.Wrap(err, "read file")
	}

	tlog.SpanFromContext(ctx).Printw("read file", "size", len(text), "name", name)

	return Compile(ctx, name, text)
}

func Compile(ctx context.Context, name string, text []byte) (obj []byte, err error) {
	f := front.New()

	f.AddFile(ctx, name, text)

	err = f.Parse(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "parse text")
	}

	p, err := f.Analyze(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "analyze")
	}

	b := back.New()

	obj, err = b.CompilePackage(ctx, nil, p)
	if err != nil {
		return nil, errors.Wrap(err, "compile")
	}

	return obj, nil
}
