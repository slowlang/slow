package set

import (
	"math/bits"

	"tlog.app/go/tlog/tlwire"
)

type (
	Bitmap struct {
		b  []uint64
		b0 [1]uint64
	}
)

func NewBitmap(len int) *Bitmap {
	s := MakeBitmap(len)
	return &s
}

func MakeBitmap(Len int) Bitmap {
	s := Bitmap{}
	s.b = s.b0[:]

	Len = (Len + 63) / 64

	if Len > len(s.b) {
		s.b = make([]uint64, Len)
	}

	return s
}

func (s *Bitmap) Set(i int) {
	i, j := s.ij(i)

	s.grow(i)

	s.b[i] |= 1 << j
}

func (s *Bitmap) Clear(i int) {
	i, j := s.ij(i)

	if i >= len(s.b) {
		return
	}

	s.b[i] &^= 1 << j
}

func (s *Bitmap) IsSet(i int) bool {
	i, j := s.ij(i)

	if i >= len(s.b) {
		return false
	}

	return (s.b[i] & (1 << j)) != 0
}

func (s *Bitmap) Or(x Bitmap) {
	s.grow(len(x.b))

	for i, x := range x.b {
		s.b[i] |= x
	}
}

func (s *Bitmap) OrCopy(x Bitmap) Bitmap {
	cp := s.Copy()
	cp.Or(x)
	return cp
}

func (s *Bitmap) And(x Bitmap) {
	for i, x := range x.b {
		if i == len(s.b) {
			break
		}

		s.b[i] &= x
	}
}

func (s *Bitmap) AndCopy(x Bitmap) Bitmap {
	cp := s.Copy()
	cp.And(x)

	return cp
}

func (s *Bitmap) AndNot(x Bitmap) {
	for i, x := range x.b {
		if i == len(s.b) {
			break
		}

		s.b[i] &^= x
	}
}

func (s *Bitmap) AndNotCopy(x Bitmap) Bitmap {
	cp := s.Copy()
	cp.AndNot(x)

	return cp
}

func (s *Bitmap) FillSet(l, r int) {
	for i := l; i < r; i++ {
		s.Set(i)
	}
}

func (s *Bitmap) Copy() Bitmap {
	r := MakeBitmap(s.Len())
	r.Or(*s)
	return r
}

func (s *Bitmap) CopyPtr() *Bitmap {
	r := NewBitmap(s.Len())
	r.Or(*s)
	return r
}

func (s *Bitmap) Size() (r int) {
	if s == nil {
		return 0
	}

	for _, c := range s.b {
		r += bits.OnesCount64(c)
	}

	return r
}

func (s *Bitmap) Reset() {
	for i := range s.b {
		s.b[i] = 0
	}
}

func (s *Bitmap) Range(f func(i int) bool) {
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

func (s *Bitmap) First() int {
	for i, x := range s.b {
		if x == 0 {
			continue
		}

		j := bits.TrailingZeros64(x)

		return i*64 + j
	}

	return -1
}

func (s *Bitmap) Last() int {
	for i := len(s.b) - 1; i >= 0; i-- {
		if s.b[i] == 0 {
			continue
		}

		j := 64 - bits.LeadingZeros64(s.b[i]) - 1

		return i*64 + j
	}

	return -1
}

func (s *Bitmap) Len() int {
	return s.Last() + 1
}

func (s Bitmap) TlogAppend(b []byte) []byte {
	var e tlwire.LowEncoder

	if s.b == nil {
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

func (s *Bitmap) ij(pos int) (i int, j int) {
	i, j = pos/64, pos%64

	return i, j
}

func (s *Bitmap) grow(i int) {
	for i >= len(s.b) {
		s.b = append(s.b, 0)
	}
}
