package bitmap

import (
	"github.com/nikandfor/tlog/tlwire"
)

type (
	Big struct {
		b  []uint64
		b0 [1]uint64
	}
)

func NewBitmap() *Big {
	s := MakeBitmap()
	return &s
}

func MakeBitmap() Big {
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

func (s Big) And(x Big) {
	for i, x := range x.b {
		if i == len(s.b) {
			break
		}

		s.b[i] &= x
	}
}

func (s Big) AndNot(x Big) {
	for i, x := range x.b {
		if i == len(s.b) {
			break
		}

		s.b[i] &^= x
	}
}

func (s Big) AndNotCp(x Big) Big {
	cp := s.Copy()
	cp.AndNot(x)

	return cp
}

func (s Big) Copy() Big {
	r := MakeBitmap()
	r.Or(s)
	return r
}

func (s Big) CopyPtr() *Big {
	r := NewBitmap()
	r.Or(s)
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

func (s Big) TlogAppend(b []byte) []byte {
	var e tlwire.LowEncoder

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
