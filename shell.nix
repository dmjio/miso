{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then miso-ghcjs.env
else miso-ghc.env.overrideAttrs (d: {  
  shellHook = ''
  '';
})
