OCaml bindings to the Google Cloud Platform APIs
================================================

## Packages

- `gcloud`: runtime-agnostic core. Resource types, and each service
  (`Batch`, `Big_query`, `Storage`, `Pub_sub`, `Kms`, ...) as a functor
  `Make (Async : Async_task_sig.S) (Client : Client_sig.S)` over an async
  runtime and an HTTP client + credential source. No Lwt dependency.
- `gcloud-lwt`: the Lwt + cohttp backend (`Async_task_lwt`,
  `Client_cohttp_lwt`), credential discovery (`Auth`, `Common`), and every
  service module instantiated for Lwt under its usual name, e.g.
  `Gcloud_lwt.Storage.get_object`. Existing callers of `Gcloud.X` become
  `Gcloud_lwt.X`.
- `gcloud-cli`: a small CLI built on `gcloud-lwt`.

To use another runtime (picos, moonpool, ...), implement the two signatures
in `gcloud` and apply the service functors yourself.

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
