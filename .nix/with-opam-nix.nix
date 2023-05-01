{ ... }: {
  perSystem = { inputs', pkgs, ... }:
    ## NOTE: The use of `../.` matters because the path is taken as relative to
    ## the current file, and therefore to `/.nix`.
    let
      scope = inputs'.opam-nix.lib.buildOpamProject {
        inherit pkgs;
        resolveArgs.with-doc = true;
        resolveArgs.with-test = true;
      } "morsmall" ../. {
        ocaml-base-compiler = "*";

        ## FIXME: Somehow, as of 2023-04-10, 1.9.6 cannot compile. So in the
        ## meantime we specify a specific version here. We should relax this
        ## as soon as things get working again.
        ocamlfind = "1.9.5";
      };
    in { packages.with-opam-nix = scope.morsmall // { inherit scope; }; };
}
