with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/b4c473f3ed6d6251ea7b8b489fc50076ac8d9b70.tar.gz";
  sha256 = "11rby2s0hxbl28a4fcwdm9lcbjfysv862xd6b9jy0rgl63dh51i3";
}) {});
with pkgs.haskell.packages;
ghcjs.callCabal2nix "app" ./. {}
