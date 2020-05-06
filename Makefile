.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune exec test/test_main.exe

.PHONY: clean
clean:
	dune clean

_opam:
	opam switch create . ocaml-base-compiler.4.08.1 --empty

opam-install-deps:
	opam install . --deps-only --working-dir --locked --with-test --yes
