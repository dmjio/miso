self: super: {

  # haskell stuff
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghcNative = super.haskell.packages.ghc9122.override {
        overrides = import ./haskell/packages/native self;
      };
      ghc9122 = super.haskell.packages.ghc9122.override {
        overrides = if super.stdenv.targetPlatform.isGhcjs
          then import ./haskell/packages/ghcjs self
          else import ./haskell/packages/ghc self;
      };
    };
  };
}
