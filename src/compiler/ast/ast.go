package ast

type (
	File struct {
		Funcs []*Func
	}

	Func struct {
		Name   string
		Params Params
		Return Params
		Body   *Block
	}

	Params []Param

	Param struct {
		Name string
		Type string
	}

	Block []Stmt

	Stmt any
)
