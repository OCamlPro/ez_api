#!/bin/sh

# This is what we would do if we needed something more:
export OPAMYES=1 OPAMVERBOSE=1

echo System OCaml version
ocaml -version
echo OPAM versions
opam --version
opam --git-version

opam init
opam switch $OCAML_VERSION

