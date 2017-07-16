{ nixpkgs ? import <nixpkgs> {}, tests ? true, haddock ? true }:
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
      cp ${drv.src}/examples/websocket/index.html $out/bin/websocket.jsexe/
      ${closurecompiler}/bin/closure-compiler $out/bin/todo-mvc.jsexe/all.js > $out/bin/todo-mvc.jsexe/min.js
      rm $out/bin/todo-mvc.jsexe/all.js
      mv $out/bin/todo-mvc.jsexe/min.js $out/bin/todo-mvc.jsexe/all.js
    '';
    checkPhase = optionalString (!isDarwin) ''
      export PATH=$PATH:${phantomjs2}/bin
      phantomjs dist/build/tests/tests.jsexe/all.js
    '';
  });
  result = {
    miso-ghcjs = buildStrictly (enableCabalFlag (enableCabalFlag miso-ghcjs "examples") "tests");
    miso-ghc = buildStrictly miso-ghc;
    release = sdistTarball (buildStrictly miso-ghc);
    s3 = nixpkgs.writeScriptBin "s3.sh" ''
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/mario.jsexe/ s3://aws-website-mario-5u38b/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/todo-mvc.jsexe/ s3://aws-website-todo-mvc-hs61i/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/websocket.jsexe/ s3://aws-website-websocket-0gx34/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/bin/router.jsexe/ s3://aws-website-router-gfy22/
       ${nixpkgs.s3cmd}/bin/s3cmd sync --recursive ${result.miso-ghcjs}/share/doc/x86_64-*-ghcjs-0.2.0-ghc7_10_3/*/html/ s3://aws-website-miso-ghcjs-1yv32/
    '';
  };
in nixpkgs.runCommand "miso" result ''
     mkdir -p $out/{lib,doc,examples}
     cp -r ${result.miso-ghcjs}/bin/* $out/examples
     cp -r ${result.miso-ghcjs}/lib/* $out/lib
     cp -r ${result.miso-ghcjs}/share/doc/* $out/doc/
     cp -r ${result.miso-ghc}/share/doc/* $out/doc/
   ''
