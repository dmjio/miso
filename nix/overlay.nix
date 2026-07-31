self: super:
let
  # nodejs' cctest suite (InspectorSocketTest.*) fails in the sandboxed
  # Intel macOS CI runner, which has no loopback networking. The compile
  # itself succeeds, so skip the test phase there. Scoped to x86_64-darwin
  # only, otherwise the overrideAttrs would change nodejs' derivation hash
  # on every platform and bust the cache.nixos.org binary cache.
  nodejsFix = super.lib.optionalAttrs (super.stdenv.hostPlatform.system == "x86_64-darwin") {
    nodejs_22 = super.nodejs_22.overrideAttrs (_: { doCheck = false; });
    nodejs = self.nodejs_22;
  };
in
nodejsFix // {

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
