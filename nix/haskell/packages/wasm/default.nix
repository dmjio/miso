pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell" {};
  miso-tests = self.callCabal2nix "miso-tests" source.miso-tests {};

  # No aeson/aeson-text variants here yet: aeson pulls in hashable, whose
  # bounds (ghc-bignum <1.4) reject the ghc-bignum-1.4 this pinned nixpkgs'
  # wasm toolchain bundles -- a real upstream version gap. The fix is
  # bumping miso's nixpkgs pin to one with a native ghc9141/ghc914 slot
  # (a bigger, separate undertaking), not overriding hashable here.

  /* examples */
  sample-app = self.callCabal2nix "app" source.sample-app {};
}
