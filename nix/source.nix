{ lib, fetchFromGitHub, fetchgit, fetchzip, ... }:
with lib;
let
  make-src-filter = src: with lib;
    cleanSourceWith {
      inherit src;
      filter =
        name: type: let baseName = baseNameOf (toString name); in
         ((type == "regular" && hasSuffix ".hs" baseName) ||
         (hasSuffix ".yaml" baseName) ||
         (hasSuffix ".cabal" baseName) ||
         (hasSuffix ".css" baseName) ||
         (hasSuffix ".html" baseName) ||
         (hasSuffix ".png" baseName) ||
         (hasSuffix ".js" baseName) ||
         (baseName == "README.md") ||
         (baseName == "LICENSE") ||
         (type == "directory" && baseName != "examples") ||
         (type == "directory" && baseName != "dist"));
    };
in
{
  sse              = make-src-filter ../examples/sse;
  miso             = make-src-filter ../.;
  examples         = make-src-filter ../examples;
  sample-app       = make-src-filter ../sample-app;
  haskell-miso     = make-src-filter ../haskell-miso.org;
  miso-from-html = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-from-html";
    rev = "8c7635889ca0a5aaac36a8b21db7f5e5ec0ae4c9";
    sha256 = "0s6kzqxbshsnqbqfj7rblqkrr5mzkjxknb6k8m8z4h10mcv1zh7j";
  };
  jsaddle = fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "0d5e427cb99391179b143dc93dfbac9c1019237b";
    sha256 = "sha256-jyJ7bdz0gNLOSzRxOWcv7eWGIwo3N/O4PcY7HyNF8Fo=";
  };
  servant-client-js = fetchFromGitHub {
    owner = "amesgen";
    repo = "servant-client-js";
    rev = "3ff9ad6906ebeeae52a7eaa31f7026790a59769a";
    hash = "sha256-7x2bxbm2cyuzhotXtdQ0jwfc0aMzjQ/fxDfHjmVvivQ=";
  };
  flatris = fetchFromGitHub {
    owner = "dmjio";
    repo = "hs-flatris";
    rev = "aa7a2e00cf87832de660718c15f1d85093ded103";
    hash = "sha256-RLBfjIGeoSTsAuxh8Pa8kcQkupVtFZEYewDE783lZFg=";
  };
  miso-plane = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-plane";
    rev = "f143eb9";
    hash = "sha256-S5urxw4eHrxsrZ9ivHeW5Nwec5eqpeat6GusNrxS+08=";
  };
  hs2048 = fetchFromGitHub {
    owner = "dmjio";
    repo = "hs2048";
    rev = "65d7b3d";
    hash = "sha256-46dhEMVutlPheLyw3/11WxF0STr9O3kR5w8xXMs9mCw=";
  };
  snake = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-snake";
    rev = "1da7afa";
    hash = "sha256-bAfIlnd3PRn9wqGn38R3+6ok1dxRR3Jeb+UUIYJZ7/M=";
  };
  microhs = fetchFromGitHub {
    owner = "augustss";
    repo = "MicroHs";
    rev = "0fc720779cfcae7b7ef1aa001a9859b7247eb211";
    hash = "sha256-cnz0rG3fkwjjIGLtyUCuGYSXYXCDS0T4qbVmGvCjwmg=";
    fetchSubmodules = true;
  };
}
