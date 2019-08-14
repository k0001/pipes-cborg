{ pkgs }:

# To be used as `packageSetConfig` for a Haskell pacakge set:
self: super:
{
  pipes-cborg = super.callPackage ./pkg.nix {};

  _shell = self.shellFor {
    packages = p: [ p.pipes-cborg ];
  };
}
