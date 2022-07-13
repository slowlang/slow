func start(argc int) (rc int) {
	a = argc
	b = 2
	return a + b + a
}

//	ADD X0, X0, X0
//	ADD X0, X0, #2

//	ADD X1, X0, #2
//	ADD X2, X1, X0
//	MOV X0, X2
