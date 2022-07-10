package main

import (
	"context"
	"fmt"
	"os"

	"github.com/nikandfor/cli"
	"github.com/nikandfor/errors"
	"github.com/nikandfor/tlog"
	"github.com/slowlang/slow/src/compiler"
	"github.com/slowlang/slow/src/compiler/parse"
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
		Commands: []*cli.Command{
			parseCmd,
			compileCmd,
		},
	}

	cli.RunAndExit(app, os.Args, os.Environ())
}

func parseAct(c *cli.Command) (err error) {
	ctx := context.Background()
	ctx = tlog.ContextWithSpan(ctx, tlog.Root())

	for _, a := range c.Args {
		x, err := parse.ParseFile(ctx, a)
		if err != nil {
			return errors.Wrap(err, "compile %v", a)
		}

		fmt.Printf("ast: %+v\n", x)
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
