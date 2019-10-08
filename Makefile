build:
	dune build

run-build:
	./_build/default/bin/main.exe build $(FILE)

run-repl:
	./_build/default/bin/main.exe repl

fmt:
	dune build @fmt --auto-promote


test1:
	./_build/default/bin/main.exe build test/hello_world.lox > test/hello_world.output
	diff test/hello_world.expected test/hello_world.output

test2:
	./_build/default/bin/main.exe build test/arithmetic.lox > test/arithmetic.output
	diff test/arithmetic.expected test/arithmetic.output

test: test1 test2
