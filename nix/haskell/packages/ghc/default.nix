pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell -fssr" {};
  miso-native = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell -fssr -fnative" {};
  miso-tests = self.callCabal2nix "miso-tests" source.miso-tests {};

  /* miso utils */
  miso-from-html = self.callCabal2nix "miso-from-html" source.miso-from-html {};

  /* examples */
  sample-app = self.callCabal2nix "app" source.sample-app {};

  /* cruft */
  crypton = dontCheck super.crypton;
  ghcid = doJailbreak super.ghcid;
  brick = doJailbreak super.brick;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;

  /* haskell-language-server on GHC 9.14 (base 4.22); mirrors HLS's own
     cabal.project allow-newer and nixpkgs haskell-updates. Drop once
     these land in the pinned nixpkgs. */
  hie-compat = doJailbreak super.hie-compat;             # base <4.22, via hiedb
  ghc-trace-events = doJailbreak super.ghc-trace-events; # base <4.22
}
