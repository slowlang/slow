package compiler

import (
	"context"
	"os"

	"tlog.app/go/errors"
	"tlog.app/go/tlog"

	"github.com/slowlang/slow/src/compiler/back"
	"github.com/slowlang/slow/src/compiler/front"
)

func CompileFile(ctx context.Context, name string) (obj []byte, err error) {
	text, err := os.ReadFile(name)
	if err != nil {
		return nil, errors.Wrap(err, "read file")
	}

	tlog.SpanFromContext(ctx).Printw("read file", "size", len(text), "path", name)

	return Compile(ctx, name, text)
}

func Compile(ctx context.Context, name string, text []byte) (obj []byte, err error) {
	f := front.New()

	err = f.AddFile(ctx, name, text)
	if err != nil {
		return nil, errors.Wrap(err, "add file")
	}

	err = f.Parse(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "parse text")
	}

	p, err := f.Compile(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "front")
	}

	b := back.New()

	obj, err = b.CompilePackage(ctx, nil, nil, p)
	if err != nil {
		return nil, errors.Wrap(err, "back")
	}

	return obj, nil
}
