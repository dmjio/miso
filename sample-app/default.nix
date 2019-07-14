with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/39b9e26ff41d6aab3b9d13a9d102ac56017f6a1f.tar.gz";
  sha256 = "1lwr35p9074b7wgz0jh4f2pjc7ls8isgzmn9xl86vb6cvsm035kf";
}) {});
with pkgs.haskell.packages;
ghcjs.callCabal2nix "app" ./. {}
