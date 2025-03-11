{ pkg ? "ghc" }:
with (import ./default.nix {});

if pkg == "ghc"
then sse-server.env.overrideAttrs (d: {  
  shellHook = ''
    alias runner="${pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c 'cabal repl'"
  '';
})
else sse-client.env
