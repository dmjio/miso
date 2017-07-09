{ nixpkgs ? import <nixpkgs> {}, tests ? true, haddock ? true }:
let
  inherit (nixpkgs.haskell.lib) buildFromSdist enableCabalFlag sdistTarball buildStrictly buildStackProject;
  inherit (nixpkgs.haskell.packages) ghc802 ghcjs ghcjsHEAD;
  inherit (nixpkgs.lib) overrideDerivation;
  inherit (nixpkgs) phantomjs2 closurecompiler s3cmd writeScriptBin fetchFromGitHub runCommand;
  miso-ghc = ghc802.callPackage ./miso-ghc.nix {};
  stack2nix = ghc802.callCabal2nix "stack2nix" (fetchFromGitHub {
    owner = "input-output-hk";
    repo = "stack2nix";
    rev = "2f83ae389e8fc8fd3a1036c211edecbcff392044";
    sha256 = "04py9m9v2hhp1n97c4l41wl9kw07x2gfqcxnnj1njfhq4xm18nfv";
  }) {};
  cabal2NixGhcjs = writeScriptBin "cabal2nix" ''
    #!/usr/bin/env sh
    ${nixpkgs.cabal2nix}/bin/cabal2nix --compiler ghcjs $@
  '';
  stack2NixEnv = runCommand "stack2nix-env" {
    shellHook = ''cd stack'';
    buildInputs = with nixpkgs; [ git cabal2NixGhcjs stack2nix cabal-install stack ];
  } ''echo only for use with nix-shell'';
  callGhcjs = x: (x.callPackage ./miso-ghcjs.nix {}).overrideDerivation (drv: {
    doCheck = tests;
    doHaddock = haddock;
    postInstall = ''
      mkdir -p $out/bin/mario.jsexe/imgs
      cp -r ${drv.src}/examples/mario/imgs $out/bin/mario.jsexe/
      cp ${drv.src}/examples/todo-mvc/index.html $out/bin/todo-mvc.jsexe/
      ${closurecompiler}/bin/closure-compiler $out/bin/todo-mvc.jsexe/all.js > $out/bin/todo-mvc.jsexe/min.js
      rm $out/bin/todo-mvc.jsexe/all.js
      mv $out/bin/todo-mvc.jsexe/min.js $out/bin/todo-mvc.jsexe/all.js
    '';
    checkPhase = ''
      export PATH=$PATH:${phantomjs2}/bin
      phantomjs dist/build/tests/tests.jsexe/all.js
    '';
  });
  miso-ghcjs = callGhcjs ghcjs;
  miso-ghcjs8 = callGhcjs ghcjsHEAD;
  stack-miso-ghcjs801 = import ./stack/ghcjs801 { pkgs = nixpkgs; };
  stack-miso-ghcjs7103 = import ./stack/ghcjs7103 { pkgs = nixpkgs; };
in {
#  inherit stack2NixEnv;
#  inherit stack-miso-ghcjs801 stack-miso-ghcjs7103;
  miso-ghcjs = buildStrictly (enableCabalFlag (enableCabalFlag miso-ghcjs "examples") "tests");
  miso-ghc = buildStrictly miso-ghc;
  release = sdistTarball (buildStrictly miso-ghc);
  miso-ghcjs8 = ghcjsHEAD.callPackage ./miso-ghcjs.nix {};
  s3 = writeScriptBin "s3.sh" ''
     ${s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs}/bin/mario.jsexe/ s3://aws-website-mario-5u38b/
     ${s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs}/bin/todo-mvc.jsexe/ s3://aws-website-todo-mvc-hs61i/
     ${s3cmd}/bin/s3cmd sync --recursive ${miso-ghcjs}/share/doc/x86_64-osx-ghcjs-0.2.0-ghc7_10_3/*/html/ s3://aws-website-miso-ghcjs-1yv32/
  '';
}
