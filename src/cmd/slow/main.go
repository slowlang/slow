package main

import (
	"context"
	"fmt"
	"os"

	"github.com/nikandfor/cli"
	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/nikandfor/tlog/ext/tlflag"
	"github.com/nikandfor/tlog/tlio"
	"github.com/slowlang/slow/src/compiler"
	"github.com/slowlang/slow/src/compiler/format"
)

func main() {
	parseCmd := &cli.Command{
		Name:   "parse",
		Action: parseAct,
		Args:   cli.Args{},
	}

	compileCmd := &cli.Command{
		Name:   "compile",
		Action: compileAct,
		Args:   cli.Args{},
	}

	app := &cli.Command{
		Name:        "slow",
		Description: "slow is a tool for managining slow source code",
		Before:      before,
		Flags: []*cli.Flag{
			cli.NewFlag("log", "stderr+dm", "log destination"),
			cli.NewFlag("v", "", "log verbosity"),
			cli.NewFlag("debug", "", "debug http address"),

			cli.HelpFlag,
			cli.FlagfileFlag,
			cli.EnvfileFlag,
		},
		Commands: []*cli.Command{
			parseCmd,
			compileCmd,
		},
	}

	err := cli.Run(app, os.Args, os.Environ())
	if err != nil {
		fmtstr := "error: %v\n"
		if tlog.If("v_error") {
			fmtstr = "error: %+v\n"
		}

		fmt.Fprintf(app.Stderr, fmtstr, err)
		os.Exit(1)
	}
}

func before(c *cli.Command) (err error) {
	w, err := tlflag.OpenWriter(c.String("log"))
	if err != nil {
		return errors.Wrap(err, "open log")
	}

	tlog.DefaultLogger = tlog.New(w)

	tlog.SetFilter(c.String("v"))

	if w, ok := w.(tlio.TeeWriter); ok {
		for _, w := range w {
			if w, ok := w.(*tlog.ConsoleWriter); ok {
				w.LevelWidth = 1
				w.Shortfile = 14
				w.MessageWidth = 20
			}
		}
	}

	return nil
}

func parseAct(c *cli.Command) (err error) {
	ctx := context.Background()
	ctx = tlog.ContextWithSpan(ctx, tlog.Root())

	for _, a := range c.Args {
		x, err := compiler.ParseFile(ctx, a)
		if err != nil {
			return errors.Wrap(err, "parse %v", a)
		}

		b, err := format.Format(ctx, nil, x)
		if err != nil {
			return errors.Wrap(err, "format %v", a)
		}

		if len(c.Args) > 1 {
			fmt.Printf("// %s\n", a)
		}

		fmt.Printf("%s", b)
	}

	return nil
}

func compileAct(c *cli.Command) (err error) {
	ctx := context.Background()
	ctx = tlog.ContextWithSpan(ctx, tlog.Root())

	for _, a := range c.Args {
		obj, err := compiler.CompileFile(ctx, a)
		if err != nil {
			return errors.Wrap(err, "compile %v", a)
		}

		fmt.Printf("%s", obj)
	}

	return nil
}
