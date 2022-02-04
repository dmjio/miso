with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.tar.gz";
}) {});
{
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
  inherit pkgs;
}

