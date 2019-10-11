build: fmt
	dune build

fmt: clean
	dune build @fmt --auto-promote

test: clean
	sh test.sh

clean:
	rm -f test/*.output

docker:
	docker build -t lox .

.PHONY: build run-build run-repl fmt test clean docker
