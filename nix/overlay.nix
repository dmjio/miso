self: super:
let
  js = import ./js super;
in
{
  # JS tooling
  inherit (js) rspeedy;

  # Reusable Lynx bundler (GHC-JS app -> .lynx.bundle). `self` is the final
  # overlayed pkgs, so it already has rspeedy/bun. See nix/lib/mk-lynx-bundle.nix.
  mkLynxBundle = import ./lib/mk-lynx-bundle.nix self;

  # haskell stuff
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghcNative = super.haskell.packages.ghc9122.override {
        overrides = import ./haskell/packages/native self;
      };
      ghc9122 = super.haskell.packages.ghc9122.override {
        overrides = if super.stdenv.targetPlatform.isGhcjs
          then import ./haskell/packages/ghcjs self
          else import ./haskell/packages/ghc self;
      };
    };
  };
}
