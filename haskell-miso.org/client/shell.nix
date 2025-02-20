with (import ../../default.nix {});
let
  inherit (pkgs.haskell.packages) ghcjs86;
  src = import ../../nix/haskell/packages/source.nix pkgs;
  client = ghcjs86.callCabal2nix "haskell-miso" (src.haskell-miso-src) {};
in
  client.env
