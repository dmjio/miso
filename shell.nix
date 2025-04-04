{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then miso-ghcjs.env
else miso-ghc.env.overrideAttrs (d: {  
  shellHook = ''
    alias runner="${pkgs.haskell.packages.ghc9101.ghcid}/bin/ghcid -c 'cabal repl'"
  '';
})
