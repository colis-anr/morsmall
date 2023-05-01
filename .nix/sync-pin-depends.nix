{ ... }: {
  perSystem = { pkgs, ... }: {
    apps.sync-pin-depends = {
      type = "app";
      program = pkgs.writeShellApplication {
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
    };
  };
}
