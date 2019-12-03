build:
	dune build

dev:
	dune build @fmt --auto-promote @install -w

test:
	./test.sh

docker:
	docker build -t lox .

.PHONY: build run-build run-repl fmt test docker
