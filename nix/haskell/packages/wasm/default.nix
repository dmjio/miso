pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nix "miso" source.miso {};
  miso-tests = self.callCabal2nix "miso-tests" source.miso-tests {};

  /* examples */
  sample-app = self.callCabal2nix "app" source.sample-app {};
}
