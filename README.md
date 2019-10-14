# Lox

Implementation of the [Lox language](http://www.craftinginterpreters.com/) in
OCaml, comprised of a front-end compiler and an interpreter.

Big thanks to [Bob Nystrom](https://github.com/munificent)!! You're amazing.

## Quickstart

Assuming `ocaml` (version `4.08.1`) and `opam` are installed and setup:

```sh
$ opam install dune base ppx_sexp_conv sexplib
$ dune build
$ ./lox run test/hello_world.lox
hello, world

$ ./lox repl
> 1+1;
2.000000
>

```


Note: the `Result` module is used which was introduced in the `4.08.1` release
hence the requirement. An earlier version can be used by using a shim: `opam
install result`.

## Implemented

- Arithmetic, strings, booleans, nil
- Statements & print
- Global variables
- Conditions (if/else)
- Boolean logic (and, or, not)
- Loops (while, for)

Note: On error, it will abort with an exception and a stacktrace of the interpreter.

## Test

`make test`

You can `opam install patdiff` for a better output, by default it uses `diff`.


## Docker

```sh
$ make docker
$ docker run -it lox
```
