

run_%: testprogs/%.out
	./$<

.PRECIOUS: testprogs/%.out
testprogs/%.out: testprogs/%.o
	ld -arch arm64 -o $@ $^ -e _start -lSystem -syslibroot `xcrun -sdk macosx --show-sdk-path`

testprogs/%.o: testprogs/%.s
	as -arch arm64 -o $@ $^ -g

.PRECIOUS: %.s
testprogs/%.s: testprogs/%.slow Makefile $(wildcard ./cmd/slow/*.go) $(wildcard ./compiler/*.go) $(wildcard ./compiler/*/*.go)
	go run ./cmd/slow compile $< | tee $@
