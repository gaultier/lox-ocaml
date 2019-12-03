build:
	dune build

dev:
	dune build @fmt --auto-promote @install -w

test: clean
	./test.sh

clean:
	rm -f test/*.output

docker:
	docker build -t lox .

.PHONY: build run-build run-repl fmt test clean docker
