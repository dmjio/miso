{ haddock ? true
, tests ? false
, examples ? false
, ios ? false
, overlays ? []
, system ? builtins.currentSystem
, crossSystem ? null
, crossOverlays ? []
}:
let
  options =
    { inherit
        haddock
        tests
        examples
        ios
        overlays
        system
        crossSystem
        crossOverlays;
    };
  pkgs = import ./nix options;
in
{
  inherit pkgs;
  miso-ghcjs = pkgs.haskell.packages.ghcjs86.miso;
  miso-ghc = pkgs.haskell.packages.ghc865.miso;
  inherit (pkgs.haskell.packages.ghc865) miso-jsaddle;
  inherit (pkgs) release release-examples;
} // pkgs.examplePkgs // pkgs.armPkgs
