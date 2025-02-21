{ pkgSrc ? builtins.fetchTarball {
    url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.7.tar.gz";
  }
}:
with (import pkgSrc {});
pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {}
