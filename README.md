# Lox

Implementation of the [Lox language](http://www.craftinginterpreters.com/) in
OCaml, comprised of a front-end compiler and an interpreter, in roughly 1KLOC !

Big thanks to [Bob Nystrom](https://github.com/munificent)!! You're amazing.

## Quickstart

Assuming `ocaml` (version `4.08.1`) and `opam` are installed and setup:

```sh
$ opam switch install 4.08.1+flambda
$ opam install dune base sexplib ppx_compare ppx_sexp_conv patdiff ocamlformat merlin
$ dune build
$ ./lox run test/hello_world.lox
hello, world

$ printf 'print 4 * 3 - 1;' | ./lox run
11

$ ./lox repl
> 1+1;
2.
> ^C

# The last field of an AST node is an internal unique id
$ echo "print 1 + 2;" | ./lox dump ast
((Print (Binary (Literal (Number 1) 1) Plus (Literal (Number 2) 2) 3) 4))

```

## Implemented

- Numbers, strings, booleans, nil
- Comments
- Number arithmetic
- Statements & print
- Variables (scope based)
- Conditions (if/else)
- Boolean logic (and, or, not)
- Loops (while, for)
- All errors are reported with line and column number
- Functions (including recursion, closures)
- Basic static analysis:
  * Return statement outside of a function body e.g `return 1;`
  * Variables assigned to themselves e.g `var a = a;`
  * Same-scope variable shadowing e.g `var a = 1; var a = 2;`

Example:

```
fun fibonacci(n) {
  if (n <= 1) return n;
  return fibonacci(n - 2) + fibonacci(n - 1);
}
for (var i = 0; i < 20; i = i + 1) {
  print fibonacci(i);
}
```

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

`lox repl` works out of the box with the [GNU Readline library](https://tiswww.cwru.edu/php/chet/readline/rltop.html) if you have it installed:

`rlwrap lox repl` will provide command history for free.
