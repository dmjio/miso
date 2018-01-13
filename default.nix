{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
, tests ? false
, haddock ? true
}:
let
  inherit (pkgs.haskell.lib) buildFromSdist enableCabalFlag sdistTarball buildStrictly;
  inherit (pkgs.haskell.packages) ghc802 ghcjs;
  inherit (pkgs.lib) overrideDerivation optionalString;
  inherit (pkgs.stdenv) isDarwin;
  inherit (pkgs) phantomjs2 closurecompiler;
  miso-ghc = ghc802.callPackage ./miso-ghc.nix { };
  miso-ghcjs = (ghcjs.callPackage ./miso-ghcjs.nix { }).overrideDerivation (drv: {
    doCheck = tests && !isDarwin;
    doHaddock = haddock;
    postInstall = ''
      cp ${drv.src}/examples/todo-mvc/index.html $out/bin/todo-mvc.jsexe/
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
  mario = ghcjs.callPackage ./examples/mario/mario.nix { miso = result.miso-ghcjs; };
  flatris = ghcjs.callCabal2nix "hs-flatris" (pkgs.fetchFromGitHub {
    repo = "hs-flatris";
    owner = "ptigwe";
    rev = "4ec1b66b2b265af96bce351538b1f604d04c6c9e";
    sha256 = "1bl9bmx8sjnh917iwk08mkq56b2jsv7fxk43z4cjngvjjl73ssqv";
  }) { miso = result.miso-ghcjs; };
  the2048 = ghcjs.callCabal2nix "2048" (pkgs.fetchFromGitHub {
    repo = "hs2048";
    owner = "dmjio";
    rev = "133eb63defa9442e64a41114d342b7d4ab6d809d";
    sha256 = "1dsbnaqv41r316hy6w6ny8c2582p95qh6xvjszkyscr2w425034w";
  }) { miso = result.miso-ghcjs; };
  snake = ghcjs.callCabal2nix "miso-snake" (pkgs.fetchFromGitHub {
    repo = "miso-snake";
    owner = "dmjio";
    rev = "aa25ee5c84cfde0ccd01b3d217485d545d5f13e5";
    sha256 = "0g85qc4pzpqillvkagirmra1axr6xn934ka82ll614cylfzbj7n1";
  }) { miso = result.miso-ghcjs; };
  result = {
    miso-ghcjs = buildStrictly (enableCabalFlag (enableCabalFlag miso-ghcjs "examples") "tests");
    miso-ghc = buildStrictly miso-ghc;
    release = sdistTarball (buildStrictly miso-ghc);
    s3 = pkgs.writeScriptBin "s3.sh" ''
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${flatris}/bin/app.jsexe/ s3://aws-website-flatris-b3cr6/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/simple.jsexe/ s3://aws-website-simple-4yic3/
       ${ pkgs.s3cmd}/bin/s3cmd sync --recursive ${mario}/bin/mario.jsexe/ s3://aws-website-mario-5u38b/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/todo-mvc.jsexe/ s3://aws-website-todo-mvc-hs61i/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/websocket.jsexe/ s3://aws-website-websocket-0gx34/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/router.jsexe/ s3://aws-website-router-gfy22/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/xhr.jsexe/ s3://aws-website-xhr-gvnhn/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/svg.jsexe/ s3://aws-website-svg-wa5mj/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/file-reader.jsexe/ s3://aws-website-file-reader-q1rpg/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/canvas2d.jsexe/ s3://aws-website-canvas-y63zw/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${snake}/bin/app.jsexe/ s3://aws-website-snake-9o0ge/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${the2048}/* s3://aws-website--6uw7z/
    '';
  };
in pkgs.runCommand "miso" result ''
     mkdir -p $out/{lib,examples}
     cp -r ${result.miso-ghcjs}/bin/* $out/examples
     mkdir -p $out/examples/mario
     cp -r ${mario}/bin/* $out/examples/mario
     cp -r ${result.miso-ghcjs}/lib/* $out/lib
     ln -s ${result.miso-ghcjs.doc} $out/ghcjs-doc
     ln -s ${result.miso-ghc.doc} $out/ghc-doc
   ''
