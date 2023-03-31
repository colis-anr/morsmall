{ ... }: {
  perSystem = { self', pkgs, config, ... }: {
    devShells.default = pkgs.mkShell {
      buildInputs = with self'.packages.morsmall.scope; [
        merlin
        ocaml-lsp-server
        ocp-indent
        utop
      ];
      inputsFrom = [ self'.packages.morsmall ];
      shellHook = config.pre-commit.installationScript;
    };
  };
}
