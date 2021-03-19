
all: build

build:
	dune build libs

dev:
	dune build

install:
	dune install

clean:
	dune clean

doc:
	dune build @doc
	rsync -ru _build/default/_doc/_html/* docs/
