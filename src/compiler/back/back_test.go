package back

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPermutate(t *testing.T) {
	t.Skip()

	var b []byte

	b = permutate(b[:0], nil)
	assert.Equal(t, `	// permutate []
`, string(b))

	b = permutate(b[:0], []Mov{{1, 0}})
	assert.Equal(t, `	// permutate [[1 0]]
	MOV	X1, X0
`, string(b))

	b = permutate(b[:0], []Mov{{1, 0}, {0, 1}})
	assert.Equal(t, `	// permutate [[1 0] [0 1]]
	// swap X0, X1
	EOR	X0, X0, X1
	EOR	X1, X0, X1
	EOR	X0, X0, X1
`, string(b))

	b = permutate(b[:0], []Mov{{1, 0}, {2, 1}, {0, 2}})
	assert.Equal(t, `	// permutate [[1 0] [2 1] [0 2]]
	// swap X0, X2
	EOR	X0, X0, X2
	EOR	X2, X0, X2
	EOR	X0, X0, X2
	// swap X2, X1
	EOR	X2, X2, X1
	EOR	X1, X2, X1
	EOR	X2, X2, X1
`, string(b))
}
