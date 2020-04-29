{ haddock ? true
, tests ? false
, examples ? false
, ios ? false
, overlays ? []
}:
with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
let
  inherit (pkgs.haskell.lib) enableCabalFlag sdistTarball buildStrictly;
  inherit (pkgs.haskell.packages) ghc865 ghcjs;
  inherit (pkgs.lib) overrideDerivation optionalString cleanSourceWith;
  inherit (pkgs) closurecompiler;
  jsaddle-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "d569be43f92b9b8c01dc3ee4c41401ab406a2076";
    sha256 = "1m1xxy4l9ii91k1k504qkxh9k1ybprm1m66mkb9dqlwcpyhcccmv";
  };
  jsaddle-dom-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle-dom";
    rev = "6ce23c5";
    sha256 = "1wpwf025czibkw6770c99zk7r30j6nh7jdzhzqbi2z824qyqzbnw";
  };
  ghcjs-dom-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "ghcjs-dom";
    rev = "b8e483a";
    sha256 = "06qlbbhjd0mlv5cymp5q0rb69a333l0fcif5zwa83h94dh25c1g7";
  };
  webkit2gtk3-javascriptcore-src = pkgs.fetchFromGitHub {
    owner = "gtk2hs";
    repo = "webkit-javascriptcore";
    rev = "5868624";
    sha256 = "0aj0cvcbnzrdi1ynahpb4034vadfrs99n5qa8dar1srhijv55g8b";
  };
  miso-src-filter = with pkgs.lib;
    cleanSourceWith {
      src = ./.;
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
  miso-examples-src-filter = with pkgs.lib;
    cleanSourceWith {
      src = ./examples;
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
  overrides = pkgs: with pkgs.haskell.lib; {
    darwin = pkgs.darwin // {
      xcode = pkgs.darwin.xcode.overrideAttrs (drv: {
        outputHash = "ec9f78b948abe341000d31f21b66051741b71163d778702b3e2e01659d60e3d2";
      });
    };
    pkgsCross = pkgs.pkgsCross // {
      iphone64 = pkgs.pkgsCross.iphone64 // {
        haskell = pkgs.pkgsCross.iphone64.haskell // {
          packages = pkgs.pkgsCross.iphone64.haskell.packages // {
            integer-simple = pkgs.pkgsCross.iphone64.haskell.packages.integer-simple // {
              ghc865 = pkgs.pkgsCross.iphone64.haskell.packages.integer-simple.ghc865.override {
                overrides = self: super: {
                  mkDerivation = args: super.mkDerivation (args // {
                    enableLibraryProfiling = false;
                    doCheck = false;
                    doHaddock = false;
                  });
                  ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${ghcjs-dom-src}/ghcjs-dom-jsaddle" {};
                  ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${ghcjs-dom-src}/ghcjs-dom-jsffi" {};
                  ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${ghcjs-dom-src}/ghcjs-dom" {};
                  jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
                  jsaddle-dom = self.callCabal2nix "jsaddle-dom" "${jsaddle-src}/jsaddle-dom" {};
                  jsaddle-wkwebview = self.callCabal2nix "jsaddle-wkwebview" "${jsaddle-src}/jsaddle-wkwebview" {};
                  servant = pkgs.lib.overrideDerivation (super.servant) (drv: {
                    postInstall = "";
                    postUnpack = ''
                      ${pkgs.gnused}/bin/sed -i '135d' servant*/servant.cabal
                      ${pkgs.gnused}/bin/sed -i '137d' servant*/servant.cabal
                    '';
                  });
                  aeson = dontCheck super.aeson;
                  QuickCheck = disableCabalFlag (super.QuickCheck) "templatehaskell";
                  miso-examples-arm = self.callCabal2nixWithOptions "miso-examples" miso-examples-src-filter "-fjsaddle -fios" {};
                  miso = pkgs.lib.overrideDerivation
                    (self.callCabal2nixWithOptions "miso" miso-src-filter "-fjsaddle -fios" {})
                    (drv: {
                      preConfigure =
                        let
                          ghc = pkgs.haskellPackages.ghcWithPackages (p: with p; [hjsmin]);
                        in
                          "${ghc}/bin/runghc minify-inline/Main.hs && mv JSBits.hs frontend-src/Miso/";
                  });
                };
              };
            };
          };
        };
      };
    };
    haskell = pkgs.haskell // {
      packages = pkgs.haskell.packages // {
        ghc864 = pkgs.haskell.packages.ghc864.override {
          overrides = self: super: with pkgs.haskell.lib; {
            happy = dontCheck (super.callHackage "happy" "1.19.9" {});
            mkDerivation = args: super.mkDerivation (args // {
              enableLibraryProfiling = false;
              doCheck = false;
              doHaddock = false;
            });
          };
        };
        ghc865 = pkgs.haskell.packages.ghc865.override {
          overrides = self: super: with pkgs.haskell.lib; rec {
            jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
            jsaddle-dom = self.callCabal2nix "jsaddle-dom" jsaddle-dom-src {};
            miso = self.callCabal2nix "miso" miso-src-filter {};
            miso-jsaddle = self.callCabal2nixWithOptions "miso" miso-src-filter "-fjsaddle" {};
            miso-examples-jsaddle = self.callCabal2nixWithOptions "miso-examples" miso-examples-src-filter "-fjsaddle" { miso = miso-jsaddle; };
            jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
            webkit2gtk3-javascriptcore = self.callCabal2nix "webkit2gtk3-javascriptcore" webkit2gtk3-javascriptcore-src {};
            jsaddle-wkwebview = self.callCabal2nix "jsaddle-wkwebview" "${jsaddle-src}/jsaddle-wkwebview" {};
            jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" "${jsaddle-src}/jsaddle-webkit2gtk" {};
            ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${ghcjs-dom-src}/ghcjs-dom-jsaddle" {};
            ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${ghcjs-dom-src}/ghcjs-dom-jsffi" {};
            ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${ghcjs-dom-src}/ghcjs-dom" {};
          };
        };
        ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
          overrides = self: super: with pkgs.haskell.lib; {
            inherit (pkgs.haskell.packages.ghc865) hpack;
            jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
            jsaddle-dom = self.callCabal2nix "jsaddle-dom" jsaddle-dom-src {};
            jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
            jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" "${jsaddle-src}/jsaddle-webkit2gtk" {};
            ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${ghcjs-dom-src}/ghcjs-dom-jsaddle" {};
            ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${ghcjs-dom-src}/ghcjs-dom-jsffi" {};
            ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${ghcjs-dom-src}/ghcjs-dom" {};
            mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
            doctest = null;
            miso-examples = (self.callCabal2nixWithOptions "miso-examples" miso-examples-src-filter "-fjsaddle" {}).overrideDerivation (drv: {
              doHaddock = haddock;
              postInstall = ''
                mkdir -p $out/bin/mario.jsexe/imgs
                cp -r ${drv.src}/mario/imgs $out/bin/mario.jsexe/
                cp ${drv.src}/xhr/index.html $out/bin/xhr.jsexe/
                ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
                  --jscomp_off=checkVars \
                  --externs=$out/bin/todo-mvc.jsexe/all.js.externs \
                  $out/bin/todo-mvc.jsexe/all.js > temp.js
                mv temp.js $out/bin/todo-mvc.jsexe/all.js
              '';
            });
            miso = (self.callCabal2nixWithOptions "miso" miso-src-filter "-ftests" {}).overrideDerivation (drv: {
              doHaddock = haddock;
              postInstall = pkgs.lib.optionalString tests ''
                ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
                  --jscomp_off=checkVars \
                  --externs=$out/bin/tests.jsexe/all.js.externs \
                  $out/bin/tests.jsexe/all.js > temp.js
                mv temp.js $out/bin/tests.jsexe/all.js
              '';
            });
          };
        };
      };
    };
  };
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { config.packageOverrides = overrides; config.allowUnfree = true; inherit overlays; };
  more-examples = import ./nix/examples.nix pkgs;
  uploadCoverage = pkgs.writeScriptBin "upload-coverage.sh" ''
    #!/usr/bin/env bash
    export PATH=$PATH:${pkgs.nodePackages.yarn}/bin
    cd tests && yarn test
    cd coverage
    ${pkgs.s3cmd}/bin/s3cmd sync --recursive lcov-report/ s3://aws-website-coverage-j7fc9/
  '';
  release = with pkgs.haskell.packages.ghc865; sdistTarball (buildStrictly miso);
  release-examples = with pkgs.haskell.packages.ghc865; sdistTarball (buildStrictly miso-examples-jsaddle);
  s3 = with pkgs.haskell.packages.ghcjs86;
       with pkgs;
    pkgs.writeScriptBin "s3.sh" ''
       ${s3cmd}/bin/s3cmd sync --recursive ${more-examples.flatris}/bin/app.jsexe/ s3://aws-website-flatris-b3cr6/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/simple.jsexe/ s3://aws-website-simple-4yic3/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/mario.jsexe/ s3://aws-website-mario-5u38b/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/todo-mvc.jsexe/ s3://aws-website-todo-mvc-hs61i/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/websocket.jsexe/ s3://aws-website-websocket-0gx34/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/router.jsexe/ s3://aws-website-router-gfy22/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/xhr.jsexe/ s3://aws-website-xhr-gvnhn/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/svg.jsexe/ s3://aws-website-svg-wa5mj/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/file-reader.jsexe/ s3://aws-website-file-reader-q1rpg/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/canvas2d.jsexe/ s3://aws-website-canvas-y63zw/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/tests.jsexe/ s3://aws-website-tests-xc9ud
       ${s3cmd}/bin/s3cmd sync --recursive ${more-examples.snake}/bin/app.jsexe/ s3://aws-website-snake-9o0ge/
       ${s3cmd}/bin/s3cmd sync --recursive ${more-examples.the2048}/* s3://aws-website--6uw7z/
    '';
   armPkgs = with pkgs; with pkgs.lib;
     optionalAttrs (ios && stdenv.isDarwin)
       { miso-arm = pkgsCross.iphone64.haskell.packages.integer-simple.ghc865.miso; };
   examplePkgs = with pkgs; with pkgs.lib;
     let
       examplePkgs = optionalAttrs examples {
         inherit (haskell.packages.ghc865) miso-examples-jsaddle;
         inherit (haskell.packages.ghcjs86) miso-examples;
         inherit s3;
        };
     in
      examplePkgs //
        optionalAttrs (stdenv.isDarwin && examples && ios)
          { inherit (pkgsCross.iphone64.haskell.packages.integer-simple.ghc865) miso-examples-arm;
          };
in
{
  inherit pkgs;
  miso-ghcjs = pkgs.haskell.packages.ghcjs86.miso;
  miso-ghc = pkgs.haskell.packages.ghc865.miso;
  inherit (pkgs.haskell.packages.ghc865) miso-jsaddle;
  inherit release;
  inherit release-examples;
} // armPkgs // examplePkgs
