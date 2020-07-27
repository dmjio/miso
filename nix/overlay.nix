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
  more-examples = { inherit (self.haskell.packages.ghcjs) flatris the2048 snake; };
  uploadCoverage = self.writeScriptBin "upload-coverage.sh" ''
    #!/usr/bin/env bash
    export PATH=$PATH:${self.nodePackages.yarn}/bin
    cd tests && yarn test
    cd coverage
    ${self.s3cmd}/bin/s3cmd sync --recursive lcov-report/ s3://aws-website-coverage-j7fc9/
  '';
  s3 = with self.haskell.packages.ghcjs86;
       with self;
    self.writeScriptBin "s3.sh" ''
       ${s3cmd}/bin/s3cmd sync --recursive ${more-examples.flatris}/bin/app.jsexe/ s3://aws-website-flatris-b3cr6/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/simple.jsexe/ s3://aws-website-simple-4yic3/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/mario.jsexe/ s3://aws-website-mario-5u38b/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/todo-mvc.jsexe/ s3://aws-website-todo-mvc-hs61i/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/websocket.jsexe/ s3://aws-website-websocket-0gx34/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/router.jsexe/ s3://aws-website-router-gfy22/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/xhr.jsexe/ s3://aws-website-xhr-gvnhn/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/svg.jsexe/ s3://aws-website-svg-wa5mj/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/file-reader.jsexe/ s3://aws-website-file-reader-q1rpg/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso-examples}/bin/canvas2d.jsexe/ s3://aws-website-canvas-y63zw/
       ${s3cmd}/bin/s3cmd sync --recursive ${miso}/bin/tests.jsexe/ s3://aws-website-tests-xc9ud
       ${s3cmd}/bin/s3cmd sync --recursive ${more-examples.snake}/bin/app.jsexe/ s3://aws-website-snake-9o0ge/
       ${s3cmd}/bin/s3cmd sync --recursive ${more-examples.the2048}/* s3://aws-website--6uw7z/
    '';
}
