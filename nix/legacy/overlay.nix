options: self: super: {

  nixosPkgsSrc =
    "https://github.com/nixos/nixpkgs/archive/6d1a044fc9ff3cc96fca5fa3ba9c158522bbf2a5.tar.gz";

  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc865 = super.haskell.packages.ghc865.override {
        overrides = import ./haskell/packages/ghc self;
      };
      ghc864 = super.haskell.packages.ghc864.override {
        overrides = selfGhc864: superGhc864: with super.haskell.lib; {
          happy = dontCheck (selfGhc864.callHackage "happy" "1.19.9" {});
          mkDerivation = args: superGhc864.mkDerivation (args // {
            enableLibraryProfiling = false;
            doCheck = false;
            doHaddock = false;
          });
        };
      };
      ghcjs86 = super.haskell.packages.ghcjs86.override {
        overrides = import ./haskell/packages/ghcjs options self;
      };
    };
  };
}
