{ pkgs ? (import ./default.nix {}).pkgs }:
let
  jsaddle-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "1e39844";
    sha256 = "1qrjrjagmrrlcalys33636w5cb67db52i183masb7xd93wir8963";
  };
  ghcARM = pkgs.pkgsCross.iphone64.ghc.override { enableIntegerSimple = true; };
in
  with pkgs.haskell.packages;
  with pkgs.haskell.lib;
{
  jsaddle = ghc865.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
  jsaddle-ghcjs = ghcjs86.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
  jsaddle-warp = dontCheck (ghc865.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
  jsaddle-warp-ghcjs = dontCheck (ghcjs86.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
  inherit ghcARM;
}
