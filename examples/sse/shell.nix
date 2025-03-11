{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then sse-client.env
else sse-server.env.overrideAttrs (d: {  
  shellHook = ''
  alias runner="${pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c 'cabal repl'"
  '';
})
