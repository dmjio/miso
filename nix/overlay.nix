options: self: super: {
  darwin = super.darwin // {
    xcode = super.darwin.xcode.overrideAttrs (drv: {
      outputHash = "ec9f78b948abe341000d31f21b66051741b71163d778702b3e2e01659d60e3d2";
    });
  };
  pkgsCross = super.pkgsCross // {
    iphone64 = super.pkgsCross.iphone64 // {
      haskell = super.pkgsCross.iphone64.haskell // {
        packages = super.pkgsCross.iphone64.haskell.packages // {
          integer-simple = super.pkgsCross.iphone64.haskell.packages.integer-simple // {
            ghc865 = super.pkgsCross.iphone64.haskell.packages.integer-simple.ghc865.override {
              overrides = import ./haskell/packages/ghcARM self;
            };
          };
        };
      };
    };
  };
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc865 = super.haskell.packages.ghc865.override {
        overrides = import ./haskell/packages/ghc865 self;
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
  more-examples = with super.haskell.lib; {
    inherit (self.haskell.packages.ghcjs) flatris the2048 snake miso-plane;
  };
}
