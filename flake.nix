{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";

    morbig.url = "github:colis-anr/morbig";
    morbig.inputs.nixpkgs.follows = "nixpkgs";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";

    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];

      imports = [
        ./.nix/with-nixpkgs.nix
        ./.nix/with-opam-nix.nix
        inputs.pre-commit-hooks.flakeModule
      ];

      perSystem = { self', pkgs, config, ... }: {
        formatter = pkgs.nixfmt;

        packages.default = self'.packages.with-nixpkgs;

        devShells.default =
          pkgs.mkShell { shellHook = config.pre-commit.installationScript; };

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
