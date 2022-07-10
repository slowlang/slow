package parse

import (
	"context"
	"fmt"
	"os"
	"reflect"

	"github.com/nikandfor/errors"

	"github.com/slowlang/slow/src/compiler/ast"
)

type (
	State struct {
		b []byte // all files concatenated

		Grammar Parser

		files []file
	}

	file struct {
		base int
		size int
		name string
	}

	Parser interface {
		Parse(ctx context.Context, b []byte, st int) (x ast.Node, i int, err error)
	}

	TypeExpectedError struct {
		T interface{}
	}

	PartialReadError struct {
		End int
	}

	stateCtxKey struct{}
)

func ParseFile(ctx context.Context, name string) (ast.Node, error) {
	data, err := os.ReadFile(name)
	if err != nil {
		return nil, errors.Wrap(err, "read file")
	}

	return Parse(ctx, data)
}

func Parse(ctx context.Context, text []byte) (x ast.Node, err error) {
	s := New()

	s.AddFile("", text)

	return s.Parse(ctx)
}

func New() *State {
	return &State{
		Grammar: &Int{},
	}
}

func (s *State) Parse(ctx context.Context) (x ast.Node, err error) {
	ctx = context.WithValue(ctx, stateCtxKey{}, &s)

	x, i, err := s.Grammar.Parse(ctx, s.b, 0)
	if err != nil {
		return nil, errors.Wrap(err, "parse as grammar")
	}

	i = SpaceAll.Skip(s.b, i)

	if i != len(s.b) {
		return x, PartialReadError{End: i}
	}

	return x, nil
}

func (s *State) AddFile(name string, text []byte) {
	f := file{
		name: name,
		base: len(s.b),
		size: len(text),
	}

	s.b = append(s.b, text...)

	s.files = append(s.files, f)
}

func (s *State) Text(pos, end int) []byte {
	return s.b[pos:end]
}

func NewTypeExpectedError(t interface{}) TypeExpectedError {
	return TypeExpectedError{
		T: t,
	}
}

func StateFromContext(ctx context.Context) *State {
	return ctx.Value(stateCtxKey{}).(*State)
}

func (e TypeExpectedError) Error() string {
	return fmt.Sprintf("%v expected", reflect.TypeOf(e.T))
}

func (e PartialReadError) Error() string {
	return fmt.Sprintf("partial read")
}
