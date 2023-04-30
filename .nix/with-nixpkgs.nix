{ ... }: {
  perSystem = { inputs', pkgs, ... }:
    let
      opkgs = pkgs.ocamlPackages;

      ## Build a Morsmall package that is based on `nixpkgs`. Morbig can be
      ## included in that treatment (with `inclMorbig = true`) in which case it
      ## also comes from `nixpkgs`; but it can also not be (with `inclMorbig =
      ## false`) in which case it comes from its own flake.
      ##
      mk-with-nixpkgs = { inclMorbig }:
        pkgs.stdenv.mkDerivation {
          name = "morsmall";
          ## NOTE: The use of `./..` matters because the path is taken as relative to
          ## the current file, and therefore to `.nix/`.
          src = ./..;

          nativeBuildInputs = with opkgs; [
            ## Basic ones, always necessary
            ocaml
            dune_3
            findlib
          ];

          propagatedBuildInputs = with opkgs; [
            (if inclMorbig then
              morbig
            else
              inputs'.morbig.packages.with-nixpkgs)

            menhirLib
            ppx_deriving
            ppx_deriving_yojson
            visitors
            yojson
          ];

          buildPhase = ''
            make build
          '';

          installPhase = ''
            make install PREFIX=$out
          '';
        };
    in {
      packages.with-nixpkgs = mk-with-nixpkgs { inclMorbig = false; };
      packages.with-nixpkgs-incl-morbig =
        mk-with-nixpkgs { inclMorbig = true; };
    };
}
