with (import ../../default.nix {});
let
  inherit (pkgs.haskell.packages) ghc865;
  src = import ../../nix/haskell/packages/source.nix pkgs;
  server = ghc865.callCabal2nix "haskell-miso" (src.haskell-miso-src) {};
in
  server.env
