pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
rec {
  /* miso */
  miso = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell -fssr" {};

  /* miso utils */
  miso-from-html = self.callCabal2nix "miso-from-html" source.miso-from-html {};
  servant-miso-html = self.callCabal2nix "servant-miso-html" source.servant-miso-html {
    miso = miso;
  };
  servant-miso-router = self.callCabal2nix "servant-miso-router" source.servant-miso-router {
    miso = miso;
    servant-miso-html = servant-miso-html;
  };

  miso-tests = self.callCabal2nix "miso-tests" source.miso-tests {
    miso = miso;
    servant-miso-router = servant-miso-router;
    servant-miso-html = servant-miso-html;
  };

  /* examples */
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${source.jsaddle}/jsaddle-warp" {});

  /* cruft */
  crypton = dontCheck super.crypton;
  ghcid = doJailbreak super.ghcid;
  brick = doJailbreak super.brick;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
}
