#!/bin/sh

export OPAMYES=1
opam switch $OCAML_VERSION
eval $(opam env)

echo Architecture
uname -a
echo OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam install --deps-only .
opam install .
opam remove .
