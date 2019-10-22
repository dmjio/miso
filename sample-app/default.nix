with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/425de4eb87e876428df4872a595ac1a0717dd165.tar.gz";
  sha256 = "1mc44vnx8qmvxkrvxhzlnrza8shnk8a0ad7hfk8hlblzrd9ha5id";
}) {});
pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {}
