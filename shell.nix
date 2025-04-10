{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs"
then miso-ghcjs.env
else miso-ghc-9122.env
else
if pkg == "micro"
then pkgs.microhs-env
else
 miso-ghc-9122.env.overrideAttrs (d: {
  shellHook = ''
    alias runner="${legacyPkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c 'cabal repl'"
  '';
})
