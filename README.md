OCaml bindings to the Google Cloud Platform APIs
================================================

## Development

The default nix devShell will have the packages needed to develop `ocaml-gcloud`:
```
nix develop '.#' # (or use nix-direnv)
dune build ...
```

### Updating opam package set

If you've updated the `.opam` files in the project and need to recalculate the `opam` deps, run `make onix-lock` and refresh the devShell:

```
# From inside the nix devShell
$ make onix-lock
$ exit # (or nix-direnv-reload if using nix-direnv)
$ nix develop '.#'
```
