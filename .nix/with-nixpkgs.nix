{ ... }: {
  perSystem = { inputs', pkgs, ... }:
    let opkgs = pkgs.ocamlPackages;
    in {
      packages.with-nixpkgs = pkgs.stdenv.mkDerivation {
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
          inputs'.morbig.packages.with-nixpkgs
          ppx_deriving
          ppx_deriving_yojson
          yojson
        ];

        buildPhase = ''
          make build
        '';

        installPhase = ''
          make install PREFIX=$out
        '';
      };
    };
}
