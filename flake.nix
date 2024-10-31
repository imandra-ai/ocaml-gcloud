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
        onixEnv = onix'.env {
          path = ./.;
          # roots = [./gcloud.opam ./gcloud-cli.opam];
          lock = ./onix-lock.json;
        };
        onixEnvDev = onix'.env {
          path = ./.;
          # roots = [./gcloud.opam ./gcloud-cli.opam];
          lock = ./onix-lock-dev.json;
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
