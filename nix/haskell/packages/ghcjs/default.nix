pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
with pkgs.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell" {};
  miso-tests = self.callCabal2nix "miso-tests" source.miso-tests {};

  ghcjs-base = self.callCabal2nix "ghcjs-base" source.ghcjs-base {};

  /* examples */
  sample-app-js = self.callCabal2nix "app" source.sample-app {};

  /* cruft */
  crypton = dontCheck super.crypton;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
}
