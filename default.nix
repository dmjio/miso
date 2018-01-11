{ nixpkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
, tests ? false
, haddock ? true
}:
let
  inherit (nixpkgs.haskell.lib) buildFromSdist enableCabalFlag sdistTarball buildStrictly;
  inherit (nixpkgs.haskell.packages) ghc802 ghcjs;
  inherit (nixpkgs.lib) overrideDerivation optionalString;
  inherit (nixpkgs.stdenv) isDarwin;
  inherit (nixpkgs) phantomjs2 closurecompiler;
  miso-ghc = ghc802.callPackage ./miso-ghc.nix { };
  miso-ghcjs = (ghcjs.callPackage ./miso-ghcjs.nix { }).overrideDerivation (drv: {
    doCheck = tests && !isDarwin;
    doHaddock = haddock;
    postInstall = ''
      mkdir -p $out/bin/mario.jsexe/imgs
      cp -r ${drv.src}/examples/mario/imgs $out/bin/mario.jsexe/
      cp ${drv.src}/examples/todo-mvc/index.html $out/bin/todo-mvc.jsexe/
      cp ${drv.src}/examples/mario/index.html $out/bin/mario.jsexe/
      cp ${drv.src}/examples/websocket/index.html $out/bin/websocket.jsexe/
      cp ${drv.src}/examples/xhr/index.html $out/bin/xhr.jsexe/
      ${closurecompiler}/bin/closure-compiler $out/bin/todo-mvc.jsexe/all.js > $out/bin/todo-mvc.jsexe/min.js
      rm $out/bin/todo-mvc.jsexe/all.js
      mv $out/bin/todo-mvc.jsexe/min.js $out/bin/todo-mvc.jsexe/all.js
    '';
    checkPhase = optionalString (!isDarwin) ''
      export PATH=$PATH:${phantomjs2}/bin
      phantomjs dist/build/tests/tests.jsexe/all.js
    '';
  });
  flatris = ghcjs.callCabal2nix "hs-flatris" (nixpkgs.fetchFromGitHub {
    repo = "hs-flatris";
    owner = "ptigwe";
    rev = "4ec1b66b2b265af96bce351538b1f604d04c6c9e";
    sha256 = "1bl9bmx8sjnh917iwk08mkq56b2jsv7fxk43z4cjngvjjl73ssqv";
  }) { miso = result.miso-ghcjs; };
  the2048 = import (nixpkgs.fetchFromGitHub {
    repo = "hs2048";
    owner = "ptigwe";
    rev = "0b4858c545d4cf6ca0c1a0cadb44d9e633c6bee6";
    sha256 = "0znr7i8q5sg3bzz8wak1l2hqs62vvpsgvj0rszbvks45vdfidnxj";
  }) {};
  snake = ghcjs.callCabal2nix "miso-snake" (nixpkgs.fetchFromGitHub {
    repo = "miso-snake";
    owner = "dmjio";
    rev = "aa25ee5c84cfde0ccd01b3d217485d545d5f13e5";
    sha256 = "0g85qc4pzpqillvkagirmra1axr6xn934ka82ll614cylfzbj7n1";
  }) { miso = result.miso-ghcjs; };
  result = {
    miso-ghcjs = buildStrictly (enableCabalFlag (enableCabalFlag miso-ghcjs "examples") "tests");
    miso-ghc = buildStrictly miso-ghc;
    release = sdistTarball (buildStrictly miso-ghc);
    s3 = nixpkgs.writeScriptBin "s3.sh" ''
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${flatris}/bin/app.jsexe/ s3://aws-website-flatris-b3cr6/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/simple.jsexe/ s3://aws-website-simple-4yic3/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/mario.jsexe/ s3://aws-website-mario-5u38b/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/todo-mvc.jsexe/ s3://aws-website-todo-mvc-hs61i/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/websocket.jsexe/ s3://aws-website-websocket-0gx34/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/router.jsexe/ s3://aws-website-router-gfy22/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/xhr.jsexe/ s3://aws-website-xhr-gvnhn/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/svg.jsexe/ s3://aws-website-svg-wa5mj/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/file-reader.jsexe/ s3://aws-website-file-reader-q1rpg/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/canvas2d.jsexe/ s3://aws-website-canvas-y63zw/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${snake}/bin/app.jsexe/ s3://aws-website-snake-9o0ge/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${the2048}/* s3://aws-website--6uw7z/
    '';
  };
in nixpkgs.runCommand "miso" result ''
     mkdir -p $out/{lib,examples}
     cp -r ${result.miso-ghcjs}/bin/* $out/examples
     cp -r ${result.miso-ghcjs}/lib/* $out/lib
     ln -s ${result.miso-ghcjs.doc} $out/ghcjs-doc
     ln -s ${result.miso-ghc.doc} $out/ghc-doc
   ''
