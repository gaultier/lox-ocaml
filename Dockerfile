FROM ocurrent/opam:debian-10-ocaml-4.08 as builder
USER root
RUN sudo apt update -y && apt install m4 -y
USER opam
RUN opam install dune base ppx_sexp_conv sexplib
RUN eval $(opam env)

WORKDIR /lox
COPY . .
RUN sudo chown opam -R /lox
ENV PATH=$PATH:/home/opam/.opam/4.08/bin/
RUN dune build

FROM alpine as runner
COPY --from=builder /lox/_build/default/bin/main.exe /usr/local/bin/lox

ENV PATH=$PATH:/usr/local/bin/
CMD ["lox", "repl"]
