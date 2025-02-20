with (builtins.fromJSON (builtins.readFile ./nix/nixpkgs.json));
{ haddock ? true
, tests ? false
, overlays ? []
, allowBroken ? false
, allowUnfree ? true
}:
let
  options = {
    inherit
      haddock tests overlays
      allowBroken allowUnfree;
  };
  pkgs = import ./nix options;
  release =
    with pkgs.haskell.packages.ghc865;
    with pkgs.haskell.lib;
    sdistTarball (buildStrictly miso);
  release-examples =
    with pkgs.haskell.packages.ghcjs;
    with pkgs.haskell.lib;
    sdistTarball (buildStrictly miso-examples);
in
{
  inherit pkgs;

  #js
  miso-ghcjs = pkgs.haskell.packages.ghcjs86.miso;
  inherit (pkgs.haskell.packages.ghcjs86) miso-examples sample-app;
  
  #native
  miso-ghc = pkgs.haskell.packages.ghc865.miso;
  miso-examples-ghc = pkgs.haskell.packages.ghc865.miso-examples;
  inherit (pkgs.haskell.packages.ghc865) sample-app-jsaddle;

  #hackage releases
  inherit release release-examples;

  #website
  inherit (pkgs)
    haskell-miso-client
    haskell-miso-server
    haskell-miso-runner;

  #ci
  deploy = pkgs.deploy rev;
}
