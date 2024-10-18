.PHONY: all build dev install clean doc deps opt-deps remove-opt-deps

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

deps:
	@opam install --deps-only .

opt-deps: deps
	@opam pin -n add websocket-httpaf.~dev https://github.com/anmonteiro/websocket-httpaf.git
	@opam pin -n add websocket-httpaf-lwt.~dev https://github.com/anmonteiro/websocket-httpaf.git
	@opam install $(shell opam show -f depopts . | sed -e 's/{.*}//g' -e 's/"//g')

remove-opt-deps:
	@opam remove $(opam show -f depopts: . | sed -e 's/{.*}//g' -e 's/"//g')
