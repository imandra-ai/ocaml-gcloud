{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    onix.url = "github:rizo/onix";
    onix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, onix }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        onix' = onix.packages.${system}.latest;
        opamFiles = [ ./gcloud.opam ./gcloud-cli.opam ];
        onixEnv = onix'.env {
          path = ./.;
          roots = opamFiles;
          lock = ./onix-lock.json;
          deps = { "ocaml-system" = "*"; };
        };
        onixEnvDev = onix'.env {
          path = ./.;
          roots = opamFiles ++ [ ./gcloud-melange.opam ];
          lock = ./onix-lock-dev.json;
          deps = {
            "ocaml-system" = "*";
            "ocaml-lsp-server" = "*";
          };
        };

      in rec {
        # for use by nix fmt
        formatter = pkgs.nixfmt-rfc-style;

        packages.gcloud-cli = onixEnv.pkgs.gcloud-cli;

        devShells.onixLock = pkgs.mkShell { buildInputs = [ onix' ]; };

        devShells.default = onixEnvDev.shell.overrideAttrs (final: prev: {
          buildInputs = prev.buildInputs ++ [ pkgs.ocamlformat_0_22_4 onix' ];
        });

        packages.dev-shell = devShells.default.inputDerivation;
      });
}
