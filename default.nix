{ pkgs ? <nixpkgs> }:
let
  config = {
    allowBroken = true;
    packageOverrides = pkgs: {
      haskell = pkgs.haskell // {
	packages = pkgs.haskell.packages // {
	  ghcjs = pkgs.haskell.packages.ghcjs.override {
	     overrides = self: super: with pkgs.haskell.lib; {
		lucid = addBuildDepend super.lucid super.semigroups;
	     };
	  };
	  ghcjsHEAD = pkgs.haskell.packages.ghcjsHEAD.override {
	     overrides = self: super: with pkgs.haskell.lib; {
		lucid = addBuildDepend super.lucid super.semigroups;
	     };
	  };
	};
      };
    };
  };
  nixpkgs = import pkgs { inherit config; };
  inherit (nixpkgs.haskell.lib) buildFromSdist enableCabalFlag;
  inherit (nixpkgs.haskell.packages) ghc802 ghcjs ghcjsHEAD;
  inherit (nixpkgs.lib) overrideDerivation;
  inherit (nixpkgs) phantomjs;
  miso-ghc = enableCabalFlag (ghc802.callPackage ./miso.nix { }) "tests";
  miso-ghcjs = (ghcjs.callPackage ./miso.nix { }).overrideDerivation (drv: {
    doCheck = true;
    doHaddock = true;
    postInstall = ''
      mkdir -p $out/bin/mario.jsexe/imgs
      cp -r ${drv.src}/examples/mario/imgs $out/bin/mario.jsexe/
      cp ${drv.src}/examples/todo-mvc/index.html $out/bin/todo-mvc.jsexe/
    '';
    checkPhase = ''
      export PATH=$PATH:${phantomjs2}/bin
      ${miso-ghc}/bin/runner dist/build/tests/tests.jsexe/all.js
    '';
  });
  miso-ghcjs8 = ghcjsHEAD.callPackage ./miso.nix { };
  result = {
    miso-ghcjs = buildFromSdist (enableCabalFlag (enableCabalFlag miso-ghcjs "examples") "tests");
  };
in nixpkgs.runCommand "miso" result ''
     mkdir -p $out/{lib,share/doc,examples}
     cp -r ${result.miso-ghcjs}/bin/* $out/examples
     cp -r ${result.miso-ghcjs}/lib/* $out/lib
     cp -r ${result.miso-ghcjs}/share/doc/* $out/share/doc/
   ''
