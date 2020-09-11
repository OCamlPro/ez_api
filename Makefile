
all: build

build:
	dune build

install:
	dune install

clean:
	dune clean

doc:
	dune build @doc
