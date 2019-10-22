with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/8de0a8a432252fe83610bfd310fc856148f524f7.tar.gz";
  sha256 = "0pmpwnss4ax2c7hcd32cs4phq3b9995wkx6qw6861q2pvqk96rhz";
}) {});
pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {}
