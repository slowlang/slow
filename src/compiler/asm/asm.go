package asm

type (
	Reg   int
	Cond  string
	Label int

	Func struct {
		Name string
		Body []Instr
	}

	Instr any

	Phi struct {
		Out [1]Reg
		In  []Reg
	}

	Imm struct {
		Out  [1]Reg
		Word uint64
	}

	Arg struct {
		Out [1]Reg
		Arg int
	}

	Add struct {
		Out [1]Reg
		In  [2]Reg
	}

	Mov struct {
		Out [1]Reg
		In  [1]Reg
	}

	Cmp struct {
		Out [1]Reg
		In  [2]Reg
	}

	B struct {
		Label Label
	}

	BCond struct {
		Cond  Cond
		Label Label
		In    [1]Reg
	}
)
