package ir

type (
	Node interface{}

	RetCode struct{}

	Halt struct{}

	Func struct {
		Clean bool
	}

	Int struct {
		Size   int
		Signed bool
	}

	Word struct {
		Value uint64
	}

	Addr struct {
		Reg int16
	}

	Move struct {
	}
)
