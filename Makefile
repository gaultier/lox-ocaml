build:
	dune build

run:
	./_build/default/bin/main.exe

fmt:
	dune build @fmt --auto-promote
