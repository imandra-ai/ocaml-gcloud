.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune build @src/local-tests

.PHONY: external-tests
external-tests:
	dune build @src/external-tests --force

.PHONY: build-external-tests
build-external-tests:
	dune build src/external_test/external_tests.exe

.PHONY: clean
clean:
	dune clean

.PHONY: submodules
submodules:
	git submodule update --init
	./prune-submodules.sh

_opam:
	opam switch create . ocaml-base-compiler.5.1.1 --empty

opam-install-deps:
	opam install ./src ./vendor/packed --deps-only --working-dir --with-test --yes

format:
	dune build @fmt --auto-promote

onix-lock:
	onix lock $(OPAM_ROOTS) --resolutions="ocaml-system=5.2.0" --lock-file ./onix-lock.json
	onix lock $(OPAM_ROOTS) ./src/gcloud-melange.opam --resolutions="ocaml-system=5.2.0,ocaml-lsp-server" --with-dev-setup=true --with-test=true --lock-file ./onix-lock-dev.json
	git add onix-lock.json onix-lock-dev.json

OPAM_ROOTS = ./src/gcloud.opam ./src/gcloud-lwt.opam ./src/gcloud-direct.opam ./src/gcloud-cli.opam ./vendor/packed/packed-error.opam ./vendor/packed/packed-error-factory.opam
