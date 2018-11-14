{ rev ? "4507926b80c6b8f73053775ffee17f6781c7e7c8"
, sha256 ? "068v9xh7d8klk62p2qwr76fyfqfh1bp08xc12x138g5q6pg6yfzb"
, pkgs ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  }) { inherit config; }
, haddock ? true
, config ? {
    packageOverrides = pkgs: with pkgs.haskell.lib; {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          ghcjs84 = pkgs.haskell.packages.ghcjs84.override {
           overrides = self: super: {
             doctest = null;
             directory-tree = dontCheck (super.directory-tree);
             http-types = dontCheck (super.http-types);
             servant = dontCheck (super.servant);
               jsaddle-warp = super.callPackage ./jsaddle-warp-ghcjs.nix {};
               ghc = super.ghc.override {
               ghcjsSrc = pkgs.fetchgit {
                   url = "https://github.com/ghcjs/ghcjs.git";
                   rev = "3959a9321a2d3e2ad4b8d4c9cc436fcfece99237";
                   sha256 = "0myjkd1scw9n4kdag7b7xdw91alaifyxw534l545hpn9a2mbaz6v";
                   fetchSubmodules = true;
                 };
               };
             };
           };
        };
     };
   };
  }
}:
let
  inherit (pkgs.haskell.lib) buildFromSdist enableCabalFlag sdistTarball buildStrictly;
  inherit (pkgs.haskell.packages) ghc843 ghcjs;
  inherit (pkgs.lib) overrideDerivation optionalString;
  inherit (pkgs.stdenv) isDarwin;
  inherit (pkgs) closurecompiler;
  miso-ghc = ghc843.callPackage ./miso-ghc.nix { };
  miso-ghcjs = (ghcjs.callPackage ./miso-ghcjs.nix { }).overrideDerivation (drv: {
    doHaddock = haddock;
    postInstall = ''
      mkdir -p $out/bin/mario.jsexe/imgs
      cp -r ${drv.src}/examples/mario/imgs $out/bin/mario.jsexe/
      cp ${drv.src}/examples/xhr/index.html $out/bin/xhr.jsexe/
      ${closurecompiler}/bin/closure-compiler $out/bin/todo-mvc.jsexe/all.js > $out/bin/todo-mvc.jsexe/min.js
      rm $out/bin/todo-mvc.jsexe/all.js
      mv $out/bin/todo-mvc.jsexe/min.js $out/bin/todo-mvc.jsexe/all.js
    '';
  });
  flatris = ghcjs.callCabal2nix "hs-flatris" (pkgs.fetchFromGitHub {
    repo = "hs-flatris";
    owner = "ptigwe";
    rev = "5b386e35db143205b4bd8d45cdf98423ed51b713";
    sha256 = "0wll5fizkdmj2hgd71v9klnnr6wxvvf36imchh2chm1slqm78zca";
  }) { miso = miso-ghcjs; };
  the2048 = ghcjs.callCabal2nix "2048" (pkgs.fetchFromGitHub {
    repo = "hs2048";
    owner = "dmjio";
    rev = "07dbed79a012240bfe19b836b6d445bb16a0602a";
    sha256 = "00rqix5g8s8y6ngxnjskvcyj19g639havn9pgpkdpxp8ni6g7xsm";
  }) { miso = miso-ghcjs; };
  snake = ghcjs.callCabal2nix "miso-snake" (pkgs.fetchFromGitHub {
    repo = "miso-snake";
    owner = "dmjio";
    rev = "c38947cd9417ab8bf8a8d3652d8bf549e35f14af";
    sha256 = "17rdc7fisqgf8zq90c3cw9c08b1qac6wirqmwifw2a0xxbigz4qc";
  }) { miso = miso-ghcjs; };
  uploadCoverage = pkgs.writeScriptBin "upload-coverage.sh" ''
    #!/usr/bin/env bash
    export PATH=$PATH:${pkgs.nodePackages.yarn}/bin
    cd tests && yarn test
    cd coverage
    ${pkgs.s3cmd}/bin/s3cmd sync --recursive lcov-report/ s3://aws-website-coverage-j7fc9/
  '';
  miso-ghcjs-release = buildStrictly (enableCabalFlag (enableCabalFlag miso-ghcjs "examples") "tests");
  release = sdistTarball (buildStrictly miso-ghc);
  s3 = pkgs.writeScriptBin "s3.sh" ''
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${flatris}/bin/app.jsexe/ s3://aws-website-flatris-b3cr6/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/simple.jsexe/ s3://aws-website-simple-4yic3/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/mario.jsexe/ s3://aws-website-mario-5u38b/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/todo-mvc.jsexe/ s3://aws-website-todo-mvc-hs61i/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/websocket.jsexe/ s3://aws-website-websocket-0gx34/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/router.jsexe/ s3://aws-website-router-gfy22/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/xhr.jsexe/ s3://aws-website-xhr-gvnhn/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/svg.jsexe/ s3://aws-website-svg-wa5mj/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/file-reader.jsexe/ s3://aws-website-file-reader-q1rpg/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/canvas2d.jsexe/ s3://aws-website-canvas-y63zw/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs-release}/bin/tests.jsexe/ s3://aws-website-tests-xc9ud
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${snake}/bin/app.jsexe/ s3://aws-website-snake-9o0ge/
       ${pkgs.s3cmd}/bin/s3cmd sync --recursive ${the2048}/* s3://aws-website--6uw7z/
    '';
  payload = pkgs.runCommand "miso" {} ''
     mkdir -p $out/{lib,examples}
     cp -r ${miso-ghcjs-release}/bin/* $out/examples
     cp -r ${miso-ghcjs-release}/lib/* $out/lib
     ln -s ${miso-ghcjs-release.doc} $out/ghcjs-doc
     ln -s ${miso-ghc.doc} $out/ghc-doc
   '';
   in { inherit payload miso-ghcjs miso-ghc release s3; }
