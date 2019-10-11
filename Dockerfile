FROM ocaml/opam2:alpine as builder
USER root
RUN sudo apk update && apk add m4
USER opam
RUN opam install dune base ppx_sexp_conv sexplib

WORKDIR /lox
COPY . .
RUN dune build

FROM alpine as runner
COPY --from=runner /lox/_build/default/bin/main.exe /usr/local/bin/lox

CMD lox repl
