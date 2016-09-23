{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghcjs" }:
    let
       pkgs = nixpkgs.pkgs.haskell.packages.${compiler};
     in
       pkgs.callPackage ./miso-todomvc.nix {
         miso-html = pkgs.callPackage ./../miso-html/miso-html.nix {};
	 miso = pkgs.callPackage ./../miso/miso.nix {
	    miso-html = pkgs.callPackage ./../miso-html/miso-html.nix {};
	 };
       }
