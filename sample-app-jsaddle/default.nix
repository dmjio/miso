with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.tar.gz";
}) {});
{
  dev = pkgs.haskell.packages.ghc8107.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {};
  inherit pkgs;
}

