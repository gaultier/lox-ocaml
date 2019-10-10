# Lox

Implementation of the [Lox language](http://www.craftinginterpreters.com/) in OCaml.
Big thanks to [Bob Nystrom](https://github.com/munificent)!! You're amazing.

## Quickstart

Assuming `ocaml` and `opam` are installed and setup:

```sh
$ opam install dune fmt base -y
$ make build
$ ./lox run test/hello_world.lox
hello, world

$ ./lox repl
> 1+1;
2.000000
>

```

## Implemented

- Arithmetic, strings, boolean logic, nil
- Statements & print
- Global variables

Note: On error, it will abort with an exception and a stacktrace of the interpreter.

## Test

`make test`

You can `opam install patdiff` for a better output, by default it uses `diff`.
