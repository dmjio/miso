{ haddock ? true
, tests ? false
}:
with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
let
  inherit (pkgs.haskell.lib) enableCabalFlags sdistTarball buildStrictly;
  inherit (pkgs.haskell.packages) ghc865 ghcjs;
  inherit (pkgs.lib) overrideDerivation optionalString cleanSourceWith;
  inherit (pkgs) closurecompiler;
  jsaddle-src = pkgs.fetchFromGitHub {
    owner = "ghcjs";
    repo = "jsaddle";
    rev = "1e39844";
    sha256 = "1qrjrjagmrrlcalys33636w5cb67db52i183masb7xd93wir8963";
  };
  servant-src = pkgs.fetchFromGitHub {
    owner = "haskell-servant";
    repo = "servant";
    rev = "d428993";
    sha256 = "1qrjrjagmrrlcalys33636w5cb67db52i183masb7xd93wir896z";
  };
  servant-src-servant = "${servant-src}/servant";
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
         (type == "directory" && baseName != "dist"));
    };
  overrides = pkgs: with pkgs.haskell.lib; {
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
                  jsaddle = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
                  jsaddle-wkwebview = self.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle-wkwebview" {};
                  servant = pkgs.lib.overrideDerivation (super.servant) (drv: {
                    postInstall = "";
                    postUnpack = ''
                      ${pkgs.gnused}/bin/sed -i '135d' servant*/servant.cabal
                      ${pkgs.gnused}/bin/sed -i '137d' servant*/servant.cabal
                    '';
                  });
                  aeson = dontCheck super.aeson;
                  QuickCheck = disableCabalFlag (super.QuickCheck) "templatehaskell";
                  miso = self.callCabal2nixWithOptions "miso" miso-src-filter "-fjsaddle -fexamples -fios" {};
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
            miso = self.callCabal2nix "miso" miso-src-filter {};
            miso-jsaddle = self.callCabal2nixWithOptions "miso" miso-src-filter "-fjsaddle -fexamples" {};
            jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
          };
        };
        ghcjs86 = pkgs.haskell.packages.ghcjs86.override {
          overrides = self: super: with pkgs.haskell.lib; {
            jsaddle = ghcjs86.callCabal2nix "jsaddle" "${jsaddle-src}/jsaddle" {};
            jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${jsaddle-src}/jsaddle-warp" {});
            mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
            doctest = null;
            miso = (ghcjs.callCabal2nixWithOptions "miso" miso-src-filter "-fexamples -ftests" {})
             .overrideDerivation (drv: {
              buildInputs = drv.buildInputs ++ [ self.jsaddle-warp self.quickcheck-instances ];
              doHaddock = haddock;
              postInstall = ''
                mkdir -p $out/bin/mario.jsexe/imgs
                cp -r ${drv.src}/examples/mario/imgs $out/bin/mario.jsexe/
                cp ${drv.src}/examples/xhr/index.html $out/bin/xhr.jsexe/
                ${closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
                  --jscomp_off=checkVars \
                  --externs=$out/bin/todo-mvc.jsexe/all.js.externs \
                  $out/bin/todo-mvc.jsexe/all.js > temp.js
                mv temp.js $out/bin/todo-mvc.jsexe/all.js
              '' + pkgs.lib.optionalString tests ''
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
  }) { config.packageOverrides = overrides; config.allowUnfree = true; };
  examples = import ./nix/examples.nix pkgs;
  uploadCoverage = pkgs.writeScriptBin "upload-coverage.sh" ''
    #!/usr/bin/env bash
    export PATH=$PATH:${pkgs.nodePackages.yarn}/bin
    cd tests && yarn test
    cd coverage
    ${pkgs.s3cmd}/bin/s3cmd sync --recursive lcov-report/ s3://aws-website-coverage-j7fc9/
  '';
  release = with pkgs.haskell.packages.ghc865; sdistTarball (buildStrictly miso);
  s3 = with pkgs.haskell.packages.ghcjs86;
       with pkgs;
    pkgs.writeScriptBin "s3.sh" ''
       ${s3cmd}/bin/s3cmd sync --recursive ${examples.flatris}/bin/app.jsexe/ s3://aws-website-flatris-b3cr6/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/simple.jsexe/ s3://aws-website-simple-4yic3/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/mario.jsexe/ s3://aws-website-mario-5u38b/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/todo-mvc.jsexe/ s3://aws-website-todo-mvc-hs61i/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/websocket.jsexe/ s3://aws-website-websocket-0gx34/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/router.jsexe/ s3://aws-website-router-gfy22/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/xhr.jsexe/ s3://aws-website-xhr-gvnhn/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/svg.jsexe/ s3://aws-website-svg-wa5mj/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/file-reader.jsexe/ s3://aws-website-file-reader-q1rpg/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/canvas2d.jsexe/ s3://aws-website-canvas-y63zw/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/tests.jsexe/ s3://aws-website-tests-xc9ud
       ${s3cmd}/bin/s3cmd sync --recursive ${examples.snake}/bin/app.jsexe/ s3://aws-website-snake-9o0ge/
       ${s3cmd}/bin/s3cmd sync --recursive ${examples.the2048}/* s3://aws-website--6uw7z/
    '';
  payload =
    with pkgs.haskell.packages.ghcjs86;
      pkgs.runCommand "miso" {} ''
        mkdir -p $out/{lib,examples}
        cp -r ${miso}/bin/* $out/examples
        cp -r ${miso}/lib/* $out/lib
        ln -s ${miso.doc} $out/ghcjs-doc
        ln -s ${pkgs.haskell.packages.ghc865.miso.doc} $out/ghc-doc
     '';
in
{
  inherit pkgs;
  miso-ghcjs = pkgs.haskell.packages.ghcjs86.miso;
  miso-ghc-jsaddle = pkgs.haskell.packages.ghc865.miso-jsaddle;
  miso-arm = pkgs.pkgsCross.iphone64.haskell.packages.integer-simple.ghc865.miso;
  inherit (pkgs.haskell.packages.ghc865) miso-jsaddle;
  inherit payload release s3;
}
