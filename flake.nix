{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";

    opam-nix.url = "github:tweag/opam-nix";
    opam-nix.inputs.nixpkgs.follows = "nixpkgs";

    morbig.url = "github:colis-anr/morbig";
    morbig.inputs.nixpkgs.follows = "nixpkgs";
    morbig.inputs.opam-nix.follows = "";
    ## NOTE: We do not use Morbig's `opam-nix`-based part of the flake here, so
    ## we can remove that input to save space and time.

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
        ./.nix/sync-pin-depends.nix
        inputs.pre-commit-hooks.flakeModule
      ];

      perSystem = { self', pkgs, config, ... }: {
        formatter = pkgs.nixfmt;

        packages.default = self'.packages.with-nixpkgs;

        devShells.default = pkgs.mkShell {
          buildInputs = (with pkgs; [ headache topiary ])
            ++ (with pkgs.ocamlPackages; [ ocaml-lsp ocp-indent ]);
          inputsFrom = [ self'.packages.default ];
          shellHook = config.pre-commit.installationScript;
        };

        pre-commit.settings.hooks = {
          nixfmt.enable = true;
          deadnix.enable = true;
          prettier.enable = true;
          dune-opam-sync.enable = true;
          opam-lint.enable = true;
          checkmake.enable = true;
          topiary.enable = true;
        };
      };

      ## NOTE: Improve the way `inputs'` are computed by also handling the case
      ## of flakes having a `lib.${system}` attribute.
      perInput = system: flake:
        if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
    };
}
