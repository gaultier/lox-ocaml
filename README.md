# Lox

Implementation of the [Lox language](http://www.craftinginterpreters.com/) in
OCaml, comprised of a front-end compiler and an interpreter.

Big thanks to [Bob Nystrom](https://github.com/munificent)!! You're amazing.

## Quickstart

Assuming `ocaml` (version `4.08.1`) and `opam` are installed and setup:

```sh
$ opam install dune base sexplib ppx_compare ppx_sexp_conv
$ dune build
$ ./lox run test/hello_world.lox
hello, world

$ printf 'print 4 * 3 - 1;' | ./lox run  -
11

$ ./lox repl
> 1+1;
2.
>

```

## Implemented

- Arithmetic, strings, booleans, nil
- Statements & print
- Global variables
- Conditions (if/else)
- Boolean logic (and, or, not)
- Loops (while, for)
- All errors are reported with line and column number
- Functions (including recursion, closures)

## Test

*Requires GNU parallel*

`make test`

You can `opam install patdiff` for a better output, by default it uses `diff`.


## Docker

```sh
$ make docker
$ docker run -it lox
```

## Better REPL experience

`lox repl` works out of the box with `rlwrap` if you have it installed:

`rlwrap lox repl` will provide command history for free.

## Development

```sh
$ opam switch install 4.09.0+flambda
$ opam install patdiff ocamlformat
```


