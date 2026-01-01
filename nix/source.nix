{ lib, fetchFromGitHub, fetchgit, fetchzip, ... }:
with lib;
with (builtins.fromJSON (builtins.readFile ../flake.lock));
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
         (type == "directory" && baseName != "tests") ||
         (type == "directory" && baseName != "dist"));
    };

  # fetch from flake
  fetchFromFlake = args:
    fetchFromGitHub {
      inherit (args.locked) owner repo rev;
      hash = args.locked.narHash;
    };

in
{
  # local sources
  miso             = make-src-filter ../.;
  miso-tests       = make-src-filter ../tests;
  sample-app       = make-src-filter ../sample-app;

  # flake sources
  jsaddle = fetchFromFlake (nodes.jsaddle);

  # non-flakified sources
  miso-from-html = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-from-html";
    rev = "8c7635889ca0a5aaac36a8b21db7f5e5ec0ae4c9";
    sha256 = "0s6kzqxbshsnqbqfj7rblqkrr5mzkjxknb6k8m8z4h10mcv1zh7j";
  };

  servant-miso-html = fetchFromGitHub {
    owner = "haskell-miso";
    repo = "servant-miso-html";
    rev = "00781d1920795b67e0476b67ed6840c388f29810";
    sha256 = "sha256-dYPlwSbQ+QXvMeS5tonBVnT9zQGADtohmD/ZAiY/cXA=";
  };

  servant-miso-router = fetchFromGitHub {
    owner = "haskell-miso";
    repo = "servant-miso-router";
    rev = "0c828e0ba30ee7a446ce8999288b32b7f6425dd1";
    sha256 = "sha256-2Vkheb2iNDFWNAToO+r8rMY3OAA6LlUtgxiCWRm0wAY=";
  };
}
