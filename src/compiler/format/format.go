package format

import (
	"context"

	"github.com/nikandfor/errors"
	"github.com/nikandfor/hacked/hfmt"

	"github.com/slowlang/slow/src/compiler/ast"
)

func Format(ctx context.Context, b []byte, x any) ([]byte, error) {
	return format(ctx, b, x, 0)
}

func format(ctx context.Context, b []byte, x any, d int) ([]byte, error) {
	switch x := x.(type) {
	case *ast.File:
		return formatFile(ctx, b, x, d)
	default:
		return nil, errors.New("unsupported type: %T", x)
	}
}

func formatFile(ctx context.Context, b []byte, x *ast.File, d int) (_ []byte, err error) {
	//	b = hfmt.AppendPrintf(b, "package %s\n", x.Package)

	for i, f := range x.Funcs {
		if i != 0 {
			b = append(b, '\n')
		}

		b, err = formatFunc(ctx, b, f, d)
		if err != nil {
			return nil, errors.Wrap(err, "func %v", f.Name)
		}
	}

	return b, nil
}

func formatFunc(ctx context.Context, b []byte, x *ast.Func, d int) ([]byte, error) {
	b = app(b, d, "func %v(", x.Name)

	for i, a := range x.Args {
		if i != 0 {
			b = append(b, ", "...)
		}

		b = app(b, 0, "%v %v", a.Name, a.Type)
	}

	b = append(b, ")"...)

	if len(x.RetArgs) != 0 {
		b = append(b, " ("...)

		for i, a := range x.RetArgs {
			if i != 0 {
				b = append(b, ", "...)
			}

			b = app(b, 0, "%v %v", a.Name, a.Type)
		}

		b = append(b, ")"...)
	}

	b = app(b, 0, " {\n")

	b, err := formatBlock(ctx, b, x.Body, d+1)
	if err != nil {
		return nil, errors.Wrap(err, "body")
	}

	b = app(b, d, "}\n")

	return b, nil
}

func formatBlock(ctx context.Context, b []byte, x *ast.Block, d int) (_ []byte, err error) {
	for _, s := range x.Stmts {
		switch s := s.(type) {
		case ast.Return:
			b = app(b, d, "return ")

			b, err = formatExpr(ctx, b, s.Value, d)
			if err != nil {
				return nil, errors.Wrap(err, "expr")
			}

			b = append(b, "\n"...)
		case ast.Assignment:
			b = app(b, d, "")

			b, err = formatExpr(ctx, b, s.Lhs, d)
			if err != nil {
				return nil, errors.Wrap(err, "lhs")
			}

			b = append(b, " = "...)

			b, err = formatExpr(ctx, b, s.Rhs, d)
			if err != nil {
				return nil, errors.Wrap(err, "rhs")
			}
			b = append(b, "\n"...)
		case *ast.IfStmt:
			b = append(b, "\n"...)
			b = app(b, d, "if ")

			b, err = formatExpr(ctx, b, s.Cond, d)
			if err != nil {
				return nil, errors.Wrap(err, "cond")
			}

			b = append(b, " {\n"...)

			b, err = formatBlock(ctx, b, s.Then, d+1)
			if err != nil {
				return nil, errors.Wrap(err, "then block")
			}

			b = app(b, d, "}\n\n")
		default:
			return nil, errors.New("unsupported stmt: %T", s)
		}
	}

	return b, nil
}

func formatExpr(ctx context.Context, b []byte, x ast.Expr, d int) (_ []byte, err error) {
	switch x := x.(type) {
	case ast.Ident:
		b = append(b, x...)
	case ast.Number:
		b = append(b, x...)
	case ast.BinOp:
		b, err = formatExpr(ctx, b, x.Left, d)
		if err != nil {
			return nil, errors.Wrap(err, "left")
		}

		b = append(b, x.Op...)

		b, err = formatExpr(ctx, b, x.Left, d)
		if err != nil {
			return nil, errors.Wrap(err, "left")
		}
	default:
		return nil, errors.New("unsupported expr: %T", x)
	}

	return b, nil
}

func app(b []byte, d int, f string, args ...any) []byte {
	const tabs = "\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t"
	b = append(b, tabs[:d]...)
	b = hfmt.AppendPrintf(b, f, args...)
	return b
}
