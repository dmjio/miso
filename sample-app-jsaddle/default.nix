with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/561ffad.tar.gz";
  sha256 = "1wwzckz2qxb873wdkwqmx9gmh0wshcdxi7gjwkba0q51jnkfdi41";
}) {});
{
  dev = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. { miso = miso-jsaddle; };
  release = pkgs.haskell.packages.ghcjs86.callCabal2nix "app" ./. {};
  inherit pkgs;
}

