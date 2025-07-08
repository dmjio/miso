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
    rev = "2513cd19184376ac8a2f0e3797a1ae7d2e522e87";
    hash = "sha256-xJ3BLiFtQmH92Y0jqIgdzJqidQHm3M1ZKHRAUEgNZF0=";
  };
  servant = fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant";
    rev = "e07e92abd62641fc0f199a33e5131de273140cb0";
    hash = "sha256-zWlU6/7MU0J/amOSZHEgVltMN9K4luNK1JV6irM9ozM=";
  };
  miso-flatris = fetchFromGitHub {
    owner = "haskell-miso";
    repo = "miso-flatris";
    rev = "36d7f2b77242beeccb0321a7b6092b7c04cf0291";
    hash = "sha256-cQz3l1lWNsU4m/db1ARwk+IGX2rQGXwYEnWT2aD+/IU=";
  };
  miso-plane = fetchFromGitHub {
    owner = "haskell-miso";
    repo = "miso-plane";
    rev = "e6199d87d9161b25daca434d679330366ad0b42a";
    hash = "sha256-TEOjj2Bb76MCWGvIRN5NhwmjEV7MZQHXnEK5szd5IqU=";
  };
  miso-2048 = fetchFromGitHub {
    owner = "haskell-miso";
    repo = "miso-2048";
    rev = "63e33d3751e3e6021d22983a08293f8c0a41cc9b";
    hash = "sha256-pZwYdtDMZm/ptnw4kFWh0lKVxJB3C5kkq6CUZ7evIR0=";
  };
  miso-snake = fetchFromGitHub {
    owner = "haskell-miso";
    repo = "miso-snake";
    rev = "46e61b0c43c2c9b61aa388418770172cdd8af8b4";
    hash = "sha256-HihtEbAnyMfNzSvJkjZeAbY/b/fm66wc4G0YuOJYfEA=";
  };
}
