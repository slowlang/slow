package parse

type (
	Spaces uint64
)

var (
	Space    = NewSpaces(' ')
	SpaceTab = NewSpaces(' ', '\t')
	SpaceAll = NewSpaces(' ', '\t', '\r', '\n')
)

func NewSpaces(skip ...byte) (ss Spaces) {
	for _, q := range skip {
		if q >= 64 {
			panic("too high char code")
		}

		ss |= 1 << q
	}

	return
}

func (s Spaces) Skip(p []byte, st int) (i int) {
	i = st

	for i < len(p) && p[i] < 64 && s&(1<<p[i]) != 0 {
		i++
	}

	return
}
