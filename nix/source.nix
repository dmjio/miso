{ lib, fetchFromGitHub, fetchgit, fetchzip, ... }:
with lib;
with (builtins.fromJSON (builtins.readFile ../flake.lock));
let
  make-src-filter =
    { src
    , excludedNames ? []
    }: with lib;
    cleanSourceWith {
      inherit src;
      filter =
        name: type:
        let
          baseName = baseNameOf (toString name);
          excluded =
            [ ".git" ".github" ".gradle" ".stack-work" "coverage"
              "dist" "dist-newstyle" "node_modules" "result"
            ] ++ excludedNames;
        in
         (!elem baseName excluded && (
         (type == "regular" && hasSuffix ".hs" baseName) ||
         (hasSuffix ".yaml" baseName) ||
         (hasSuffix ".cabal" baseName) ||
         (hasSuffix ".css" baseName) ||
         (hasSuffix ".c" baseName) ||
         (hasSuffix ".html" baseName) ||
         (hasSuffix ".png" baseName) ||
         (hasSuffix ".js" baseName) ||
         (baseName == "README.md") ||
         (baseName == "CHANGELOG.md") ||
         (baseName == "LICENSE") ||
         (type == "directory")));
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
  # Keep package sources independent: changing a sample or native host must not
  # invalidate the Miso library derivation.
  miso = make-src-filter {
    src = ../.;
    excludedNames = [ "sample-app" "sample-app-native" "tests" ];
  };
  miso-tests = make-src-filter { src = ../tests; };
  sample-app = make-src-filter { src = ../sample-app; };
  sample-app-native = make-src-filter {
    src = ../sample-app-native;
    excludedNames = [ "android" "ios" "build" "styles.css" "conformance.css" ];
  };

  # non-flakified sources
  miso-from-html = fetchFromGitHub {
    owner = "dmjio";
    repo = "miso-from-html";
    rev = "8c7635889ca0a5aaac36a8b21db7f5e5ec0ae4c9";
    sha256 = "0s6kzqxbshsnqbqfj7rblqkrr5mzkjxknb6k8m8z4h10mcv1zh7j";
  };

  ghcjs-base = fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-base";
    rev = "b7ae2cafb082ff247d29ab50d651f4e955ada47a";
    sha256 = "0az7xdvd5irsf4x18dgz8aqymrk01bxzmjwhwrmbqmi8kdp1dfb4";
  };
}
