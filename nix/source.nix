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
    rev = "36454332d2a2b289bb892279a354851d68c566b3";
    hash = "sha256-daNn2I6BlDxYTzkULyUiOcsQRF1Jn9Ub+VFdYJPCA2A=";
  };
  miso-plane = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-plane";
    rev = "0b7153a0174e2df456188a38084ba5cbbbdd7243";
    hash = "sha256-WThKRb1k1kpVlcdkNtIDHneko1Mt0KEecq8x4sV944g=";
  };
  hs2048 = fetchFromGitHub {
    owner = "dmjio";
    repo = "hs2048";
    rev = "eb6e1b6f85546607d3f0f150ec480ceee69668ba";
    hash = "sha256-DelAE0ArLXan1Gx52qsKO9umfvJAW2xtY2UuNZkJQUY=";
  };
  snake = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-snake";
    rev = "4a1c7701973682d0df227185273ddbcbb2459e54";
    hash = "sha256-hqEWoOvvraYUPFN71ofGpwXQLNRgZV5OCiMkdHNYF2A=";
  };
}
