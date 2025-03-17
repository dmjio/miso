with (import ../default.nix {});
{
  inherit pkgs;
  sample-app-js = pkgs.haskell.packages.ghcjs.callCabal2nix "app" ./. {};
  sample-app = pkgs.haskell.packages.ghc865.callCabal2nix "app" ./. {};
}
