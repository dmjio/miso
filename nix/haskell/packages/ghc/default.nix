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

  /* hls

     nixpkgs does not test HLS against GHC 9.14 (ghc9141 is absent from
     `released` in pkgs/top-level/release-haskell.nix), so the chain still
     carries pre-9.14 bounds. `ghc-trace-events` and `hie-compat` only have a
     stale `base < 4.22`, so a jailbreak is enough. The formatter/linter
     plugins are a different story: they pull ghc-lib-parser-9.12, a snapshot
     of GHC 9.12's own sources, which will not compile against base-4.22 and
     containers-0.8. Switch those plugins off and the whole subtree
     (ghc-lib-parser, hlint, ormolu, fourmolu, stylish-haskell) leaves the
     closure. Core IDE features are unaffected; format-on-save and hlint
     hints are lost. */
  haskell-language-server =
    # `pkgs.haskell.lib` takes the derivation first; `.compose` is the
    # pipe-friendly order.
    let compose = pkgs.haskell.lib.compose;
    in pkgs.lib.pipe super.haskell-language-server (
      [
        # nixpkgs adds apply-refact/hlint/refact unconditionally for ghc >
        # 9.11, which would drag ghc-lib-parser back in regardless of the
        # flags below.
        (compose.overrideCabal (drv: { buildDepends = []; }))
      ] ++ map compose.disableCabalFlag [
        "hlint"
        "ormolu"
        "fourmolu"
        "stylishHaskell"
      ]
    );

  /* cruft */
  crypton = dontCheck super.crypton;
  ghcid = doJailbreak super.ghcid;
  ghc-trace-events = doJailbreak super.ghc-trace-events;
  hie-compat = doJailbreak super.hie-compat;
  brick = doJailbreak super.brick;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
}
