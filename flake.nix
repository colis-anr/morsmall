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
      imports = [
        inputs.pre-commit-hooks.flakeModule
        ./.nix/devshell-default.nix
        ./.nix/package-morsmall.nix
        ./.nix/package-default.nix
        ./.nix/perinput-lib.nix
        ./.nix/pre-commit-settings.nix
        ./.nix/systems.nix
      ];

      perSystem = { pkgs, ... }: { formatter = pkgs.nixfmt; };
    };
}
