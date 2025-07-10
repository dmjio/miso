pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nix "miso" source.miso {};

  /* deps */
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  ghcjs-base = self.callCabal2nix "ghcjs-base" source.ghcjs-base {};

  /* cruft */
  crypton = dontCheck super.crypton;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
  servant-server = doJailbreak super.servant-server;

  /* Includes BigInt patch to support the jsbi polyfill, for Quick/PrimJS */
  ghc = super.ghc.overrideAttrs (drv: drv // {
    patches = (drv.patches or []) ++ [ ../../../patches/jsbi.patch ];
  });

}

