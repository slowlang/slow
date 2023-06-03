package bitmap

import (
	"math/bits"

	"github.com/nikandfor/tlog/tlwire"
)

type (
	Big struct {
		b  []uint64
		b0 [1]uint64
	}
)

func New() *Big {
	s := Make()
	return &s
}

func Make() Big {
	s := Big{}
	s.b = s.b0[:]

	return s
}

func (s *Big) Set(i int) {
	i, j := s.ij(i)

	s.grow(i)

	s.b[i] |= 1 << j
}

func (s Big) Clear(i int) {
	i, j := s.ij(i)

	if i >= len(s.b) {
		return
	}

	s.b[i] &^= 1 << j
}

func (s Big) IsSet(i int) bool {
	i, j := s.ij(i)

	if i >= len(s.b) {
		return false
	}

	return (s.b[i] & (1 << j)) != 0
}

func (s *Big) Or(x Big) {
	s.grow(len(x.b))

	for i, x := range x.b {
		s.b[i] |= x
	}
}

func (s *Big) OrCopy(x Big) Big {
	cp := s.Copy()
	cp.Or(x)
	return cp
}

func (s Big) And(x Big) {
	for i, x := range x.b {
		if i == len(s.b) {
			break
		}

		s.b[i] &= x
	}
}

func (s Big) AndCopy(x Big) Big {
	cp := s.Copy()
	cp.And(x)

	return cp
}

func (s Big) AndNot(x Big) {
	for i, x := range x.b {
		if i == len(s.b) {
			break
		}

		s.b[i] &^= x
	}
}

func (s Big) AndNotCopy(x Big) Big {
	cp := s.Copy()
	cp.AndNot(x)

	return cp
}

func (s *Big) FillSet(l, r int) {
	for i := l; i < r; i++ {
		s.Set(i)
	}
}

func (s Big) Copy() Big {
	r := Make()
	r.Or(s)
	return r
}

func (s Big) CopyPtr() *Big {
	r := New()
	r.Or(s)
	return r
}

func (s *Big) Size() (r int) {
	if s == nil {
		return 0
	}

	for _, c := range s.b {
		r += bits.OnesCount64(c)
	}

	return r
}

func (s Big) Reset() {
	for i := range s.b {
		s.b[i] = 0
	}
}

func (s Big) Range(f func(i int) bool) {
	for i, x := range s.b {
		if x == 0 {
			continue
		}

		//	for j := bits.TrailingZeros64(x); j < bits.Len64(x); j++ {
		for j := 0; j < 64; j++ {
			if (x & (1 << j)) == 0 {
				continue
			}

			if !f(i*64 + j) {
				return
			}
		}
	}
}

func (s Big) First() int {
	for i, x := range s.b {
		if x == 0 {
			continue
		}

		j := bits.TrailingZeros64(x)

		return i*64 + j
	}

	return -1
}

func (s *Big) TlogAppend(b []byte) []byte {
	var e tlwire.LowEncoder

	if s == nil {
		return e.AppendNil(b)
	}

	b = e.AppendTag(b, tlwire.Array, -1)

	s.Range(func(i int) bool {
		b = e.AppendInt(b, i)

		return true
	})

	b = e.AppendBreak(b)

	return b
}

func (s Big) ij(pos int) (i int, j int) {
	i, j = pos/64, pos%64

	return i, j
}

func (s *Big) grow(i int) {
	for i >= len(s.b) {
		s.b = append(s.b, 0)
	}
}
