{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, opam-nix, opam-repository }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        fs = pkgs.lib.fileset;
        on = opam-nix.lib.${system};
        ocaml-version = "5.1.1";
        ocaml-base-compiler = ocaml-version;

        # List of opam files you which to be read by opam-nix
        opamFiles = [
          ./gcloud.opam
          ./gcloud-cli.opam

          # Also include the opam files to any submoduled dependencies
          # ./vendor/some-lib/some-lib.opam
        ];

        opamFilePackageNames = (map (x:
          (pkgs.lib.removeSuffix ".opam" (builtins.baseNameOf (toString x))))
          opamFiles);

        # attrset containing all opam packages defined in the opamFiles list
        # for a specific OCaml version
        opamScope = (on.buildOpamProject' {
          repos = [ opam-repository ];
          resolveArgs.with-test = true;
          recursive = true;
          overlays = on.__overlays ++ [
            (final: prev:
              builtins.foldl' (acc: pkg:
                (acc // {
                  # Allows us to treat the whole repo as a single unit and `dune build` it together,
                  # without opam-nix attempting to build each opam package individually.
                  # Similar to opam install --deps-only
                  ${pkg} =
                    prev.${pkg}.overrideAttrs (oa: { dontBuild = true; });
                })) {
                  # other-package-override = prev.other-package-overide.overrideAttrs (oa: {
                  #   nativeBuildInputs = oa.nativeBuildInputs ++ (with pkgs; [ makeWrapper git ]);
                  # });
                } opamFilePackageNames)
          ];
        } (fs.toSource {
          root = ./.;
          fileset = fs.unions opamFiles;
        }) { inherit ocaml-base-compiler; });

        gcloud-cli = pkgs.stdenv.mkDerivation {
          pname = "gcloud-cli";
          version = "1.0.0";
          buildInputs = (map (p: opamScope.${p}) opamFilePackageNames);
          buildPhase = ''
            dune build @install -p gcloud,gcloud-cli
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp _build/install/default/bin/* $out/bin/
          '';

          src = (fs.toSource {
            root = ./.;

            # Prevent changes to the nix flake file from busting the build cache.
            # Add any other files which the build should ignore to this list.
            fileset =
              fs.difference ./. (fs.unions [ ./flake.nix ./flake.lock ]);
          });
        };

      in rec {
        # for use by nix fmt
        formatter = pkgs.nixfmt-rfc-style;

        packages.gcloud-cli = gcloud-cli;

        packages.default = pkgs.mkShell {
          dontDetectOcamlConflicts = true;
          buildInputs = (map (p: opamScope.${p}) opamFilePackageNames) ++ [
            pkgs.ocaml-ng.ocamlPackages_5_1.utop
            pkgs.ocaml-ng.ocamlPackages_5_1.ocaml-lsp
            pkgs.ocamlformat_0_22_4
          ];
        };
      });
}
