pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell" {};

  /* miso utils */
  miso-from-html = self.callCabal2nix "miso-from-html" source.miso-from-html {};

  /* examples */
  sample-app = self.callCabal2nix "app" source.sample-app {};
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${source.jsaddle}/jsaddle-warp" {});
  servant = self.callCabal2nix "servant" "${source.servant}/servant" {};

  /* hls */
  haskell-language-server = doJailbreak (
    self.callCabal2nix "haskell-language-server" source.haskell-language-server {}
  );

  stylish-haskell = doJailbreak (
    self.callCabal2nix "stylish-haskell" source.stylish-haskell {}
  );

  /* cruft */
  crypton = dontCheck super.crypton;
  ghcid = doJailbreak super.ghcid;
  brick = doJailbreak super.brick;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
  servant-server = doJailbreak super.servant-server;
}
