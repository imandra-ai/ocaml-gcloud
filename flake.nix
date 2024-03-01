{
  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";

    #ocamlformat 0.22.4 is a bit old, so is only in older versions of nixpkgs
    nixpkgs2305.url = "nixpkgs/nixos-23.05";

    flake-utils.url = "github:numtide/flake-utils";
    opam-nix = {
      url = "github:tweag/opam-nix";
      inputs.opam-repository.follows = "opam-repository";
    };
    opam-repository = {
      url = "github:ocaml/opam-repository";
      flake = false;
    };
    imandra-opam-repository = {
      url =
        "github:imandra-ai/opam-repository/5b5590df81404a6e2fc586dd9cb6a2f969d417b2";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs2305, flake-utils, opam-nix, opam-repository, imandra-opam-repository }@inputs:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          pkgs2305 = nixpkgs2305.legacyPackages.${system};
          fs = pkgs.lib.fileset;
          on = opam-nix.lib.${system};
          ocaml-version = "5.1.0";
          ocaml-base-compiler = ocaml-version;

          # List of opam files you which to be read by opam-nix
          opamFiles = [
            ./my-ocaml-package.opam

            # Also include the opam files to any submoduled dependencies
            # ./vendor/some-lib/some-lib.opam
          ];

          opamFilePackageNames = (map
            (x:
              (pkgs.lib.removeSuffix ".opam" (builtins.baseNameOf (toString x))))
            opamFiles);


          # attrset containing all opam packages defined in the opamFiles list
          # for a specific OCaml version
          opamScope = (on.buildOpamProject'
            {
              repos = [ opam-repository ];
              recursive = true;
              overlays = on.__overlays ++ [
                (final: prev:
                  builtins.foldl'
                    (acc: pkg:
                      (acc // {
                        # Allows us to treat the whole repo as a single unit and `dune build` it together,
                        # without opam-nix attempting to build each opam package individually.
                        # Similar to opam install --deps-only
                        ${pkg} =
                          prev.${pkg}.overrideAttrs (oa: { dontBuild = true; });
                      }))
                    {
                      # other-package-override = prev.other-package-overide.overrideAttrs (oa: {
                      #   nativeBuildInputs = oa.nativeBuildInputs ++ (with pkgs; [ makeWrapper git ]);
                      # });
                    }
                    opamFilePackageNames)
              ];
            }
            (fs.toSource {
              root = ./.;
              fileset = fs.unions opamFiles;
            })
            { inherit ocaml-base-compiler; });

          # build any dev deps on the correct ocaml version, without conflicting with project deps.
          devOpamScope = (on.queryToScope { repos = [ opam-repository ]; } {
            inherit ocaml-base-compiler;
            ocaml-lsp-server = "*";
            utop = "*";
          });


          # imandra-extract must be on OCaml 4.12.1
          imandra-extract = (on.queryToScope
            {
              repos = [ opam-repository imandra-opam-repository ];
            }
            {
              "imandra-extract" = "1.1.0-860.47af51daf06019394dd46cd7854cbc24ce038100";
              ocaml-base-compiler = "4.12.1";
            }).imandra-extract;

        in


        rec {
          # for use by nix fmt
          formatter = pkgs.nixpkgs-fmt;

          packages.opamScope = opamScope;
          packages.imandra-extract = imandra-extract;

          packages.default = pkgs.mkShell {
            buildInputs =
              (map (p: packages.opamScope.${p}) opamFilePackageNames) ++ [
                devOpamScope.utop
                devOpamScope.ocaml-lsp-server

                #ocamlformat 0.22.4 is a bit old, so is only in older versions of nixpkgs
                pkgs2305.ocamlformat_0_22_4
              ];

            # imandra-extract needs to be added to the path manually as below, rather than
            # to the package list above, otherwise you get clashes between the two
            # OCaml versions (some LIB related env vars are also set for `buildInputs`).
            shellHook = ''
              PATH=${packages.imandra-extract}/bin:$PATH
            '';
          };


          # build with nix build '.?submodules=1#my-ocaml-package'
          packages.my-ocaml-package = pkgs.stdenv.mkDerivation {
            pname = "my-ocaml-package";
            version = "1.0.0";
            buildInputs = (map (p: packages.opamScope.${p}) opamFilePackageNames);
            buildPhase = ''
              dune build @install -p my-ocaml-package
            '';

            installPhase = ''
              echo Nothing to do here yet...
              mkdir -p $out/
              # mkdir -p $out/bin
              # cp _build/install/default/bin/* $out/bin/
            '';

            src = (fs.toSource {
              root = ./.;

              # Prevent changes to the nix flake file from busting the build cache.
              # Add any other files which the build should ignore to this list.
              fileset = fs.difference ./. (fs.unions [
                ./flake.nix
                ./flake.lock
              ]);
            });
          };
        });
}
