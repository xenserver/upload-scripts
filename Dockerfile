FROM ocaml/opam:ubuntu

RUN mkdir tmp/
RUN mkdir tmp/src/

COPY opam _tags Makefile tmp/
COPY src/update_xs_yum.ml tmp/src/

WORKDIR tmp/

RUN opam pin add --no-action update_xs_yum .

RUN opam depext -y update_xs_yum

RUN opam install -y update_xs_yum
