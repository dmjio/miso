with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/4e15d92cc4f37605fc398229695de4d903ef8a47.tar.gz";
  sha256 = "1lwr35p9074b7wgz0jh4f2pjc7ls8isgzmn9xl86vb6cvsm035kf";
}) {});
with pkgs.haskell.packages;
ghcjs.callCabal2nix "app" ./. {}
