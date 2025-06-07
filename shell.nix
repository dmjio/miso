{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then miso-ghcjs.env
else
if pkg == "micro"
then pkgs.microhs-env
else miso-ghc-9122.env
