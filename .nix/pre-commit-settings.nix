{ ... }: {
  perSystem = { ... }: {
    pre-commit.settings.hooks = {
      nixfmt.enable = true;
      deadnix.enable = true;
      prettier.enable = true;
      dune-opam-sync.enable = true;
      opam-lint.enable = true;
    };
  };
}
