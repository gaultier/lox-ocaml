build:
	dune build

run-build:
	./_build/default/bin/main.exe build $(FILE)

run-repl:
	./_build/default/bin/main.exe repl

fmt:
	dune build @fmt --auto-promote
