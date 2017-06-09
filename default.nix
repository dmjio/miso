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
  miso-ghc = nixpkgs.haskell.packages.ghc802.callPackage ./miso.nix { };
  miso-ghcjs = nixpkgs.haskell.packages.ghcjs.callPackage ./miso.nix { };
  miso-ghcjs8 = nixpkgs.haskell.packages.ghcjsHEAD.callPackage ./miso.nix { };
in with nixpkgs.haskell.lib; {
  miso-ghc = buildFromSdist miso-ghc;
  miso-ghcjs = buildFromSdist (enableCabalFlag miso-ghcjs "examples");
}
