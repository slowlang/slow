package back

import "github.com/slowlang/slow/src/compiler/set"

type BitsInt = set.Bits[int]

func sliceSet[S ~[]E, E any, I interface{ ~int }](s S, i I, x E) S {
	var z E

	for int(i) >= len(s) {
		s = append(s, z)
	}

	s[i] = x

	return s
}

func max[K ~int](x, y K) K {
	if x >= y {
		return x
	}

	return y
}
