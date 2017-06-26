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
  inherit (nixpkgs) phantomjs2 closurecompiler;
  miso-ghc = ghc802.callPackage ./miso-ghc.nix { };
  miso-ghcjs = (ghcjs.callPackage ./miso-ghcjs.nix { }).overrideDerivation (drv: {
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
  miso-ghcjs8 = ghcjsHEAD.callPackage ./miso-ghcjs.nix { };
  result = {
    miso-ghcjs = buildFromSdist (enableCabalFlag (enableCabalFlag miso-ghcjs "examples") "tests");
    miso-ghc = buildFromSdist miso-ghc;
    miso-ghcjs8 = buildFromSdist miso-ghcjs8;
  };
in nixpkgs.runCommand "miso" result ''
     mkdir -p $out/{lib,doc,examples}
     cp -r ${result.miso-ghcjs}/bin/* $out/examples
     cp -r ${result.miso-ghcjs}/lib/* $out/lib
     cp -r ${result.miso-ghcjs}/share/doc/* $out/doc/
     cp -r ${result.miso-ghc}/share/doc/* $out/doc/
     cp -r ${result.miso-ghcjs8}/share/doc/* $out/doc/
   ''
