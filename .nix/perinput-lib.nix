{ ... }: {
  ## Improve the way `inputs'` are computed by also handling the case of
  ## flakes having a `lib.${system}` attribute.
  ##
  perInput = system: flake:
    if flake ? lib.${system} then { lib = flake.lib.${system}; } else { };
}
