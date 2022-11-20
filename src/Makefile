

run_%: %.out
	./$<

.PRECIOUS: %.out
%.out: %.o
	ld -arch arm64 -o $@ $^ -e _start -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path`

%.o: %.s
	as -arch arm64 -o $@ $^ -g

.PRECIOUS: %.s
%.s: %.slow Makefile $(wildcard ./cmd/slow/*.go) $(wildcard ./compiler/*.go) $(wildcard ./compiler/*/*.go)
	go run ./cmd/slow compile $< >$@
