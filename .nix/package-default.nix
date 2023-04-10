{ ... }: {
  perSystem = { self', ... }: { packages.default = self'.packages.morsmall; };
}
