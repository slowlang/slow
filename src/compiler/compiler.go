package compiler

import (
	"context"
	"os"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"

	"github.com/slowlang/slow/src/compiler/analyze"
	"github.com/slowlang/slow/src/compiler/compile"
	"github.com/slowlang/slow/src/compiler/parse"
)

func CompileFile(ctx context.Context, name string) (obj []byte, err error) {
	text, err := os.ReadFile(name)
	if err != nil {
		return nil, errors.Wrap(err, "read file")
	}

	tlog.SpanFromContext(ctx).Printw("read file", "size", len(text), "name", name)

	return Compile(ctx, name, text)
}

func Compile(ctx context.Context, name string, text []byte) (obj []byte, err error) {
	st := parse.New()

	st.AddFile(name, text)

	x, err := st.Parse(ctx)
	if err != nil {
		tlog.SpanFromContext(ctx).Printw("abstract syntax tree", "x_type", tlog.FormatNext("%T"), x, "x", x, "err", err)
		return nil, errors.Wrap(err, "parse text")
	}

	tlog.SpanFromContext(ctx).Printw("abstract syntax tree", "x_type", tlog.FormatNext("%T"), x, "x", x)

	y, err := analyze.Analyze(ctx, st, x)
	if err != nil {
		return nil, errors.Wrap(err, "analyze ast")
	}

	tlog.SpanFromContext(ctx).Printw("intermediate representation", "y_type", tlog.FormatNext("%T"), y, "y", y)

	obj, err = compile.Compile(ctx, y)
	if err != nil {
		return nil, errors.Wrap(err, "compile")
	}

	return obj, nil
}
