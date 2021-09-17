with (builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json));
{ haddock ? true
, tests ? false
, examples ? false
, ios ? false
, overlays ? []
, system ? builtins.currentSystem
, crossSystem ? null
, crossOverlays ? []
, allowBroken ? false
, allowUnfree ? true
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
        crossOverlays
        allowBroken
        allowUnfree;
    };
  pkgs = import ./nix options;
  armPkgs =
    with pkgs; with pkgs.lib;
     optionalAttrs (options.ios && stdenv.isDarwin)
       { miso-arm = pkgsCross.iphone64.haskell.packages.integer-simple.ghc865.miso; };
  release =
    with pkgs.haskell.packages.ghc865;
    with pkgs.haskell.lib;
    sdistTarball (buildStrictly miso);
  release-examples =
    with pkgs.haskell.packages.ghc865;
    with pkgs.haskell.lib;
    sdistTarball (buildStrictly miso-examples-jsaddle);
   examplePkgs = with pkgs; with pkgs.lib;
     let
       examplePkgs = optionalAttrs options.examples {
         inherit (haskell.packages.ghc865) miso-examples-jsaddle;
         inherit (haskell.packages.ghcjs86) miso-examples;
        };
     in
      examplePkgs //
        optionalAttrs (stdenv.isDarwin && options.examples && options.ios)
          { inherit (pkgsCross.iphone64.haskell.packages.integer-simple.ghc865) miso-examples-arm;
          };
in
{
  deploy = pkgs.deploy rev;
  inherit pkgs;
  miso-ghcjs = pkgs.haskell.packages.ghcjs86.miso;
  miso-ghc = pkgs.haskell.packages.ghc865.miso;
  inherit (pkgs.haskell.packages.ghc865) miso-jsaddle;
  # inherit release release-examples;
} // examplePkgs // armPkgs
