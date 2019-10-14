with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/4e15d92cc4f37605fc398229695de4d903ef8a47.tar.gz";
  sha256 = "0kg9lg18qdsnb6gw3jmnl49widzf5cy151bls7k09d5dxlm0myd8";
}) {});
with pkgs.haskell.packages;
ghc865.callCabal2nix "app" ./. { miso = ghc865.miso-jsaddle; }
