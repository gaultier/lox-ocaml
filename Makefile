build: fmt
	dune build

run:
	./_build/default/bin/main.exe run $(FILE)

run-repl:
	./_build/default/bin/main.exe repl

fmt: clean
	dune build @fmt --auto-promote

test: clean
	sh test.sh

clean:
	rm -f test/*.output

.PHONY: build run-build run-repl fmt test clean
