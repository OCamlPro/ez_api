
all: build

build:
	dune build
install:
	dune install
clean:
	dune clean

doc:
	dune build @doc

-include autoconf/Makefile.config
-include ocp-autoconf.d/Makefile

ocp-build-conf:
	ocp-autoconf

ocp-build: ocp-build-build $(PROJECT_BUILD)

ocp-build-install: ocp-build-install $(PROJECT_INSTALL)

ocp-build-clean: ocp-build-clean $(PROJECT_CLEAN)

ocp-build-distclean: clean ocp-distclean $(PROJECT_DISTCLEAN)
	find . -name '*~' -exec rm -f {} \;

-include autoconf/Makefile.rules
