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
  miso-ghc = ghc802.callPackage ./miso.nix { };
  miso-ghcjs = (ghcjs.callPackage ./miso.nix { }).overrideDerivation (drv: {
    doHaddock = true;
    postInstall = ''
      mkdir -p $out/bin/mario.jsexe/imgs
      cp -r ${drv.src}/examples/mario/imgs $out/bin/mario.jsexe/
      cp ${drv.src}/examples/todo-mvc/index.html $out/bin/todo-mvc.jsexe/
    '';
  });
  miso-ghcjs8 = ghcjsHEAD.callPackage ./miso.nix { };
  result = {
    miso-ghc = buildFromSdist miso-ghc;
    miso-ghcjs = buildFromSdist (enableCabalFlag miso-ghcjs "examples");
  };
in nixpkgs.runCommand "miso" result ''
     mkdir -p $out/{lib,share/doc,examples}
     cp -r ${result.miso-ghcjs}/bin/* $out/examples
     cp -r ${result.miso-ghcjs}/lib/* $out/lib
     cp -r ${result.miso-ghcjs}/share/doc/* $out/share/doc/
     cp -r ${result.miso-ghc}/lib/* $out/lib
     cp -r ${result.miso-ghc}/share/doc/* $out/share/doc/

   ''
