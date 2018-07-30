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
	opam switch create . --empty

.PHONY: opam-pins
opam-pins:
	@echo 'Pin ocb-stubblr to fix nocrypto in utop (see https://github.com/mirleft/ocaml-nocrypto/issues/115 and https://github.com/pqwy/ocb-stubblr/pull/11)'
	opam pin add -y --no-action ocb-stubblr https://github.com/hannesm/ocb-stubblr.git\#fix

	@echo 'Pin Nocrypto for RSA PKCS signing (https://github.com/mirleft/ocaml-nocrypto/commit/04c6c717faa074c7876e14fd0ebcf4cbd6ab24a3)'
	opam pin add -y --no-action nocrypto --dev-repo

	@echo 'Pin JWT for RSA256 support'
	opam pin add -y --no-action jwt https://github.com/AestheticIntegration/ocaml-jwt.git#rsa256

	@echo
	@echo "You may need to run \`opam upgrade\` if you've updated these."

.PHONY: opam-install
opam-install: opam-pins
	opam install .
