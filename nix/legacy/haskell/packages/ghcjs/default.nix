options: pkgs:
let
  source = import ../../../../source.nix pkgs;
in
with pkgs.haskell.lib;
with pkgs.lib;
self: super:
{
  sample-app-js = self.callCabal2nix "app" source.sample-app {};
  miso-tests = self.callCabal2nix "miso-tests" source.miso-tests {};
  miso-prod = self.callCabal2nixWithOptions "miso" source.miso "-fproduction" {};
  miso = self.callCabal2nix "miso" source.miso {};
  mkDerivation = args: super.mkDerivation (args // { doCheck = false; });

  # cruftachu
  doctest = null;
}
