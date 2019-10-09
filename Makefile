build: fmt
	dune build

fmt: clean
	dune build @fmt --auto-promote

test: clean
	sh test.sh

clean:
	rm -f test/*.output

.PHONY: build run-build run-repl fmt test clean
