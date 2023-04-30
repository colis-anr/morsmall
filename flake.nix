{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";

    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";

    ## We add an input for the OPAM repository. We don't actually need it, but
    ## this allows us to control when opam-nix's repository gets updated.
    opam-repository.url = "github:ocaml/opam-repository";
    opam-repository.flake = false;
    opam-nix.inputs.opam-repository.follows = "opam-repository";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [
        inputs.pre-commit-hooks.flakeModule
        ./.nix/devshell-default.nix
        ./.nix/package-morsmall.nix
        ./.nix/package-default.nix
      ];

      perSystem = { pkgs, ... }: {
        formatter = pkgs.nixfmt;

        pre-commit.settings.hooks = {
          nixfmt.enable = true;
          deadnix.enable = true;
          prettier.enable = true;
          dune-opam-sync.enable = true;
          opam-lint.enable = true;
        };
      };

      ## NOTE: Improve the way `inputs'` are computed by also handling the case
      ## of flakes having a `lib.${system}` attribute.
      perInput = system: flake:
        if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };
}
