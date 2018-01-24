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
    rev = "5b386e35db143205b4bd8d45cdf98423ed51b713";
    sha256 = "0wll5fizkdmj2hgd71v9klnnr6wxvvf36imchh2chm1slqm78zca";
  }) { miso = result.miso-ghcjs; };
  the2048 = ghcjs.callCabal2nix "2048" (pkgs.fetchFromGitHub {
    repo = "hs2048";
    owner = "dmjio";
    rev = "07dbed79a012240bfe19b836b6d445bb16a0602a";
    sha256 = "00rqix5g8s8y6ngxnjskvcyj19g639havn9pgpkdpxp8ni6g7xsm";
  }) { miso = result.miso-ghcjs; };
  snake = ghcjs.callCabal2nix "miso-snake" (pkgs.fetchFromGitHub {
    repo = "miso-snake";
    owner = "dmjio";
    rev = "1d2962e71c8f83d997381a7d7bcede11c2d73821";
    sha256 = "06l2ajw9g5x5c45hwkdyzm0x0ngg16f7n9aqnyng1q46w5wr4sa8";
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
