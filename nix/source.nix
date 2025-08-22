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
  sample-app       = make-src-filter ../sample-app;

  # flake sources
  jsaddle = fetchFromFlake (nodes.jsaddle);
  servant = fetchFromFlake (nodes.servant);

  # non-flakified sources
  miso-from-html = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-from-html";
    rev = "8c7635889ca0a5aaac36a8b21db7f5e5ec0ae4c9";
    sha256 = "0s6kzqxbshsnqbqfj7rblqkrr5mzkjxknb6k8m8z4h10mcv1zh7j";
  };
}
