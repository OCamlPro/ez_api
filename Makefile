
all: build

build:
	@dune build src

dev:
	@dune build

install:
	@dune install

clean:
	@dune clean

doc:
	@dune build @doc
	@rsync -ru _build/default/_doc/_html/* docs/

build-tests:
	@opam install ocurl websocket-lwt-unix js_of_ocaml
	@dune build test
	