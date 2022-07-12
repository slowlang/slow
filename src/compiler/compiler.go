package compiler

import (
	"context"
	"os"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/slowlang/slow/src/compiler/front"
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
	st := front.New()

	st.AddFile(ctx, name, text)

	err = st.Parse(ctx)
	if err != nil {
		//	tlog.SpanFromContext(ctx).Printw("abstract syntax tree", "x_type", tlog.FormatNext("%T"), x, "x", x, "err", err)
		return nil, errors.Wrap(err, "parse text")
	}

	err = st.Analyze(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "analyze")
	}

	obj, err = st.Compile(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "compile")
	}

	return obj, nil
}
