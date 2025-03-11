{ pkg ? "ghc" }:

with (import ../default.nix {});

if pkg == "ghc"
then miso-examples-ghc.env.overrideAttrs (d: {  
  shellHook = ''
    export PATH=$PATH:${pkgs.ghciwatch}/bin
    function watch () {
      ghciwatch \
        --command "cabal repl $1" \
        --watch $1 \
        --test-ghci=Main.main
    }
  '';
  })
else 
  miso-examples-ghcjs.env
