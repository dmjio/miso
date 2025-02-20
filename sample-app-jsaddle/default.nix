with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.7.tar.gz";
}) {});
{
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. { inherit miso; };
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
  inherit pkgs;
}

