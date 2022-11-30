package arm64

type (
	Reg  int
	Addr uint64

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

	MOV struct {
		Out [1]Reg
		In  [1]Word
	}
)
