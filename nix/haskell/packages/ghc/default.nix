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

  /* miso utils */
  miso-from-html = self.callCabal2nix "miso-from-html" source.miso-from-html {};

  /* examples */
  sample-app = self.callCabal2nix "app" source.sample-app {};

  /* hls */
  haskell-language-server = self.callCabal2nix "haskell-language-server" source.haskell-language-server {};
  stylish-haskell = doJailbreak super.stylish-haskell;
  ghc-lib-parser = doJailbreak super.ghc-lib-parser;

  /* cruft */
  crypton = dontCheck super.crypton;
  ghcid = doJailbreak super.ghcid;
  brick = doJailbreak super.brick;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
}
