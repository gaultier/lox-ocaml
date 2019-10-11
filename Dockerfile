FROM ocaml/opam2:alpine as builder
USER root
RUN sudo apk update && apk add m4
USER opam
RUN opam install dune base ppx_sexp_conv sexplib

WORKDIR /lox
COPY . .
RUN sudo chown opam -R /lox
RUN eval `opam env` && dune build --profile=release

FROM alpine as runner
COPY --from=builder /lox/_build/default/bin/main.exe /usr/local/bin/lox

ENV PATH=$PATH:/usr/local/bin/
CMD ["lox", "repl"]
