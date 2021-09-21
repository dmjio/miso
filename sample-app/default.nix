with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.tar.gz";
}) {});
pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {}
