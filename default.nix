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
  deploy = pkgs.deploy rev;
  miso-ghcjs = pkgs.haskell.packages.ghcjs86.miso;
  miso-ghc = pkgs.haskell.packages.ghc865.miso;
  inherit (pkgs.haskell.packages.ghcjs86) miso-examples sample-app;
  inherit (pkgs.haskell.packages.ghc865) sample-app-jsaddle;
  inherit release release-examples;
  inherit (pkgs) haskell-miso;
}
