
all: build

build:
	dune build

install:
	dune install

clean:
	dune clean

doc:
	dune build @doc
	rsync -r _build/default/_doc/_html/* docs/
