package ir

type (
	Flag int
	Reg  int

	ADD struct {
		Out [1]Reg
		In  [2]Reg
	}

	SUB struct {
		Out [1]Reg
		In  [2]Reg
	}

	CMP struct {
		OutFlags []Flag
		In       [2]Reg
	}
)
