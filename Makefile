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

format:
	dune build @fmt --auto-promote

onix-lock:
	onix lock ./gcloud.opam ./gcloud-cli.opam --resolutions="ocaml-system=5.2.0" --lock-file ./onix-lock.json
	onix lock ./gcloud.opam ./gcloud-cli.opam ./gcloud-melange.opam --resolutions="ocaml-system=5.2.0,ocaml-lsp-server" --with-dev-setup=true --with-test=true --lock-file ./onix-lock-dev.json
	git add onix-lock.json onix-lock-dev.json
