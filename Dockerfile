FROM ocaml/opam2:alpine as builder
USER root
RUN sudo apk update && apk add m4
USER opam

WORKDIR /lox
COPY . .
RUN sudo chown opam -R /lox
RUN opam switch 4.09
RUN opam install .

FROM alpine as runner
COPY --from=builder /home/opam/.opam/4.09/bin/lox /usr/local/bin/lox

ENV PATH=$PATH:/usr/local/bin/
CMD ["lox", "repl"]
