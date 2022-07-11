package grammar

import (
	"bufio"
	"bytes"
	"context"

	"github.com/nikandfor/errors"
)

type (
	Grammar struct {
		m map[string]int
	}

	rule struct {
		Name   string
		Parser Parser
		Func   func()
	}

	Parser interface {
	}
)

func New(ctx context.Context, text []byte) (*Grammar, error) {
	s := bufio.NewScanner(bytes.NewReader(text))

	lnum := 0
	for s.Scan() {
		lnum++

		line := s.Bytes()
		i := skipSpaces(line, 0)

		if i == len(line) || line[i] == '#' {
			continue
		}

		i = findChar(line, i, '=')
		if i == len(line) {
			return nil, errors.New("no equal sign in line: %v", lnum)
		}

		name := string(bytes.TrimSpace(line[:i]))

		i = skipSpaces(line, i+1)

		for i < len(line) {
			switch line[i] {
			case '"':
				end := findChar(line, i+1, '"')
				if end == len(line) {
					return nil, errors.New("unended string: %d:%d", lnum, i)
				}

				_ = line[i+1 : end]

				i = end + 1
			default:
				return nil, errors.New("unexpected token: %d:%d", lnum, i)
			}

			i = skipSpaces(line, i)
		}

		_ = name
	}

	if err := s.Err(); err != nil {
		return nil, errors.Wrap(err, "scanner")
	}

	return nil, nil
}

func (g *Grammar) parseRule(ctx context.Context, b []byte, i int) (Parser, error) {
}

func skipSpaces(b []byte, i int) int {
	for i < len(b) && b[i] == ' ' {
		i++
	}

	return i
}

func findChar(b []byte, i int, c byte) int {
	for i < len(b) && b[i] != c {
		i++
	}

	return i
}
