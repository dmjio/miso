let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources.haskellNix {};
  args = haskellNix.nixpkgsArgs // { config.allowUnfree = true; };
  pkgs = import haskellNix.sources.nixpkgs-unstable args;
  miso = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "miso";
      src = ./.;
    };
    compiler-nix-name = "ghc8105";
  };
  examples = pkgs.haskell-nix.project {
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "miso-examples";
      src = ./examples;
    };
    compiler-nix-name = "ghc8105";
  };
in
{
  inherit pkgs;
  miso = miso.projectCross.ghcjs.hsPkgs.miso.components.library;
}


