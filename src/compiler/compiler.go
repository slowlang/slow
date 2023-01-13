package compiler

import (
	"context"
	"os"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"

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

	f.AddFile(ctx, name, text)

	err = f.Parse(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "parse text")
	}

	p, err := f.Compile(ctx)
	if err != nil {
		return nil, errors.Wrap(err, "analyze")
	}

	//	for _, f := range p.Funcs {
	//		tlog.Printw("func", "name", f.Name, "in", f.In, "out", f.Out)
	//
	//		for id, e := range f.Code {
	//			tlog.Printw("expr", "id", id, "type", tlog.FormatNext("%T"), e, "val", e)
	//		}
	//	}

	b := back.New()

	obj, err = b.CompilePackage(ctx, nil, nil, p)
	if err != nil {
		return nil, errors.Wrap(err, "compile")
	}

	return obj, nil
}
