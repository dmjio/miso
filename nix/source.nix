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
              "dist" "dist-newstyle" "dist-mcabal" ".mcabal" ".emcache" "node_modules" "result"
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
    rev = "b25d91bb51241d496adfb2b1f8f8dd593efe646b";
    sha256 = "1wi3vpswik9wj8x16ik5v17sjkd537c482kiavh719kfaz4h7884";
  };

  # MicroHs (mhs) with the JSVal / JavaScript FFI, browser targets and
  # js-sources support (dmjio/MicroHs, branch rts-standalone), plus the
  # packages miso needs when built with mhs.  See nix/mhs/default.nix.
  microhs = fetchFromGitHub {
    owner = "dmjio";
    repo = "MicroHs";
    rev = "d58bb4ebd825f29470e7047d61cc4adc7e95d67f";
    hash = "sha256-P7EN+sxcUcl6nw6EV128CYxhhtyoXKMy6FVIm7pc71M=";
  };
  mhs-ghc-compat = fetchFromGitHub {
    owner = "augustss";
    repo = "ghc-compat";
    rev = "e04bd70e4040d52cd1d319387bda7514000b58a0";
    hash = "sha256-YRmgq3v/V3ewX9I1czFxPNfQ1r9c9YUV0MCMUPN/HpE=";
  };
  mhs-array = fetchFromGitHub {
    owner = "augustss";
    repo = "array-mhs";
    rev = "cbc2dbc31a84c7bcebef4a15b045e4ce21b27984";
    hash = "sha256-5qIch+MNsOMyP3pDRC4/mgGUX33y67eG0Bhtgq7sE+E=";
  };
  # containers 0.8 has MicroHs support (__MHS__)
  mhs-containers = fetchFromGitHub {
    owner = "haskell";
    repo = "containers";
    rev = "0c3b9ee93af23f86fb8ad5ffe7729cb82fc4cd3a";
    hash = "sha256-I8QOxeTPw1sSfSiFaaLqDRZPLP1j7rvusZRDzxzZ1fI=";
  };
  mhs-transformers = fetchzip {
    url = "https://hackage.haskell.org/package/transformers-0.6.1.2/transformers-0.6.1.2.tar.gz";
    sha256 = "1hyqi74hamb9cry60r7i4l62ml2rbn2agrkmzav4bqznmfwz097w";
  };
  mhs-mtl = fetchzip {
    url = "https://hackage.haskell.org/package/mtl-2.3.1/mtl-2.3.1.tar.gz";
    sha256 = "0mrh1n5i1d00rslrjwj8fvnfjpsjx6aswixa93bx6v94kxlkkakh";
  };
}
