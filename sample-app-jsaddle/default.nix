with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/fcfdb46.tar.gz";
  sha256 = "17xiclymc6jjjc2klra0xgawylgn61bv45jp5mqsagd4dd0j0rb5";
}) {});
{
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
  inherit pkgs;
}

