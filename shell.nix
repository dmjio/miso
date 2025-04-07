{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then miso-ghcjs.env
else miso-ghc.env.overrideAttrs (d: {  
  shellHook = ''
    alias runner="${legacyPkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c 'cabal repl'"
  '';
})
