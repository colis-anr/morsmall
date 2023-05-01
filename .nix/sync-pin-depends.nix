{ ... }: {
  perSystem = { pkgs, ... }:
    let
      sync-pin-depends = pkgs.writeShellApplication {
        name = "sync-pin-depends";
        text = ''
          printf "Finding revision or Morbig in \`flake.lock\`... "
          rev=$(jq -r .nodes.morbig.locked.rev flake.lock)
          printf 'done! got %s.\n' "$rev"
          printf "Writing it in \`morsmall.opam.template\`... "
          sed -i "s|morbig#\(.*\)\"|morbig#$rev\"|" morsmall.opam.template
          printf 'done.\n'
        '';
        runtimeInputs = with pkgs; [ jq ];
      };
    in {
      apps.sync-pin-depends = {
        type = "app";
        program = sync-pin-depends;
      };

      pre-commit.settings.hooks.sync-pin-depends = {
        enable = true;
        entry = "${sync-pin-depends}/bin/sync-pin-depends";
        files = "(flake\\.lock$)|(\\.opam\\.template$)";
        pass_filenames = false;
      };
    };
}
