package back

import "github.com/slowlang/slow/src/compiler/set"

type BitsInt = set.Bits[int]

func sliceSet[K set.Key, V any](s *[]V, k K, v V) {
	var z V

	for int(k) >= len(*s) {
		*s = append(*s, z)
	}

	(*s)[k] = v
}

func sliceResize[V any](s *[]V, l int) {
	var z V

	for l >= cap(*s) {
		*s = append((*s)[:cap(*s)], z)
	}

	*s = (*s)[:l]
}

func max[K ~int](x, y K) K {
	if x >= y {
		return x
	}

	return y
}
