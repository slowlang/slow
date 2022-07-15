package ast

type (
	Ident  string
	Number string
	Op     string

	File struct {
		Package string

		Funcs []*Func
	}

	Func struct {
		Name    string
		Args    Args
		RetArgs Args
		Body    *Block
	}

	Args []Arg

	Arg struct {
		Num  int
		Name Ident
		Type string
	}

	Block struct {
		Stmts []Stmt
	}

	VarDecl struct {
		Name  string
		Value Expr
	}

	Assignment struct {
		Pos int
		Lhs Expr
		Rhs Expr
	}

	Return struct {
		Pos   int
		Value Expr
	}

	IfStmt struct {
		Pos  int
		Cond Expr
		Then *Block
		Else *Block
		Elif *IfStmt
	}

	BinOp struct {
		Op    Op
		Left  Expr
		Right Expr
	}

	Stmt interface{}
	Expr interface{}
)
