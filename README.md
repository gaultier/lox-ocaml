# Lox

Implementation of the [Lox language](http://www.craftinginterpreters.com/) in OCaml.
Big thanks to [Bob Nystrom](https://github.com/munificent)!! You're amazing.

## Quickstart

```sh
$ brew install ocaml dune
$ opam install base
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
