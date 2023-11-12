package set

import (
	"math/bits"

	"github.com/nikandfor/tlog/tlwire"
)

type (
	Key interface {
		~int | ~int64
	}

	Bits[K Key] struct {
		base K
		b    []uint64
		b0   [2]uint64
	}
)

var zeros = [8]uint64{}

func MakeBits[K Key](base K) Bits[K] {
	s := Bits[K]{
		base: base,
	}

	s.b = s.b0[:]

	return s
}

func (s Bits[K]) Copy() Bits[K] {
	c := MakeBits(s.base)

	c.grow(len(s.b))
	copy(c.b, s.b)

	return c
}

func (s *Bits[K]) Set(k K) {
	i, j := s.ij(k)

	s.grow(i)

	s.b[i] |= 1 << j
}

func (s Bits[K]) IsSet(k K) bool {
	i, j := s.ij(k)

	if i >= len(s.b) {
		return false
	}

	return s.b[i]&(1<<j) != 0
}

func (s Bits[K]) Clear(k K) {
	i, j := s.ij(k)

	if i >= len(s.b) {
		return
	}

	s.b[i] &^= 1 << j
}

func (s *Bits[K]) SetAll(k ...K) {
	for _, k := range k {
		s.Set(k)
	}
}

func (s *Bits[K]) Merge(x Bits[K]) {
	if s.base != x.base {
		panic(s)
	}

	s.grow(len(x.b))

	for i, x := range x.b {
		s.b[i] |= x
	}
}

func (s Bits[K]) Intersect(x Bits[K]) {
	if s.base != x.base {
		panic(s)
	}

	n := len(s.b)
	if m := len(x.b); m < n {
		n = m
	}

	for i, x := range x.b[:n] {
		s.b[i] &= x
	}
}

func (s Bits[K]) Substract(x Bits[K]) {
	if s.base != x.base {
		panic(s)
	}

	n := len(s.b)
	if m := len(x.b); m < n {
		n = m
	}

	for i, x := range x.b[:n] {
		s.b[i] &^= x
	}
}

func (s Bits[K]) Size() (r int) {
	for _, c := range s.b {
		r += bits.OnesCount64(c)
	}

	return r
}

func (s Bits[K]) Range(f func(i K) bool) {
	for i, x := range s.b {
		if x == 0 {
			continue
		}

		for j := bits.TrailingZeros64(x); j < bits.Len64(x); j++ {
			//for j := 0; j < 64; j++ {
			if (x & (1 << j)) == 0 {
				continue
			}

			if !f(s.base + K(i*64+j)) {
				return
			}
		}
	}
}

func (s Bits[K]) TlogAppend(b []byte) []byte {
	var e tlwire.LowEncoder

	if s.b == nil {
		return e.AppendNil(b)
	}

	b = e.AppendTag(b, tlwire.Array, -1)

	s.Range(func(k K) bool {
		b = e.AppendInt(b, int(k))

		return true
	})

	b = e.AppendBreak(b)

	return b
}

func (s *Bits[K]) Reset() {
	for i := 0; i < len(s.b); {
		i += copy(s.b[i:], zeros[:])
	}

	s.Strip()
}

func (s *Bits[K]) Strip() {
	l := len(s.b)

	for l > 0 && s.b[l-1] == 0 {
		l--
	}

	s.b = s.b[:l]
}

func (s *Bits[K]) ij(k K) (i int, j int) {
	p := int(k - s.base)
	i, j = p/64, p%64

	return i, j
}

func (s *Bits[K]) grow(i int) {
	if s.b == nil {
		s.b = s.b0[:]
	}

	for i >= cap(s.b) {
		s.b = append(s.b[:cap(s.b)], 0)
	}

	s.b = s.b[:cap(s.b)]
}
