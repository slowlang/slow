

.PHONY: run
run_%: %.out
	./$<

%.out: %.o
	ld -arch arm64 -o $@ $^ -e _start -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path`

%.o: %.s
	as -arch arm64 -o $@ $^

%.s: %.slw
	go run ./cmd/slow compile $^ >$@
