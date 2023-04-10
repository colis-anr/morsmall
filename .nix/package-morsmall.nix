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
        merlin = "*";
        ocaml-base-compiler = "*";
        ocaml-lsp-server = "*";
        ocp-indent = "*";
        utop = "*";

        ## FIXME: Somehow, as of 2023-04-10, 1.9.6 cannot compile. So in the
        ## meantime we specify a specific version here. We should relax this
        ## as soon as things get working again.
        ocamlfind = "1.9.5";
      };
    in { packages.morsmall = scope.morsmall // { inherit scope; }; };
}
