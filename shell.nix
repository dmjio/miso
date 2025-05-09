{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then miso-ghcjs.env
else miso-ghc-9122.env