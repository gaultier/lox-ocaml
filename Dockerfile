FROM ocaml/opam2:alpine as builder
USER root
RUN sudo apk update && apk add m4
USER opam

WORKDIR /lox
COPY . .
RUN sudo chown opam -R /lox
RUN opam install .
CMD ["lox", "repl"]
