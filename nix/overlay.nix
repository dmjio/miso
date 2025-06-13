self: super: {

  # ghci watcher
  ghciwatch =
    (builtins.getFlake "github:MercuryTechnologies/ghciwatch")
      .outputs.packages."${super.system}".ghciwatch;

  # dmj: ensure you call 'bun run test' first
  # js nix packaging is more trouble than its worth right now
  coverage = self.stdenv.mkDerivation {
    name = "coverage";
    src = ../coverage;
    buildCommand = ''
      mkdir -p $out
      cp -v $src/* $out
    '';
  };

  # dmj: Ensure you call 'nix-shell --run 'cabal haddock-project'' first
  # this happens in CI
  haddocks = self.stdenv.mkDerivation {
    name = "haddocks";
    src = ../haddocks;
    buildCommand = ''
      mkdir -p $out
      cp -rv $src/* $out
    '';
  };

  # haskell stuff
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc9122 = super.haskell.packages.ghc9122.override {
        overrides = if super.stdenv.targetPlatform.isGhcjs
          then import ./haskell/packages/ghcjs self
          else import ./haskell/packages/ghc self;
      };
      ghc9101 = super.haskell.packages.ghc9101.override {
        overrides = import ./haskell/packages/ghc self;
      };
    };
  };

  microhsTargetsConf =
    super.writeTextFile {
      name = "targets.conf";
      text = ''
        [default]
        cc = "gcc"
        ccflags = ""
        conf = "unix-64"

        [emscripten]
        cc = "emcc"
        ccflags = "-sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sSINGLE_FILE -sENVIRONMENT=shell -sWASM=0"
        conf = "unix-64"
      '';
    };

  microhs =
    super.stdenv.mkDerivation {
      name = "MicroHs";
      src = (import ../nix/source.nix super).microhs;
      buildInputs = with super; [ emscripten ];
      installPhase = ''
        mkdir -p $out/{bin,share,lib/bin}

        cp -v ./bin/mcabal $out/bin
        cp -v ./bin/cpphs $out/bin
        cp -v ./bin/mhs $out/bin
        cp -rv ./lib $out

        cp -rv ./generated $out
        cp -rv ./boards $out
        cp -rv ./paths $out
        cp -rv ./doc $out
        cp -rv ./src $out
        cat ${self.microhsTargetsConf} > $out/targets.conf

        cp README.md $out/share
      '';
    };

  microhs-env = super.mkShell {
    name = "microhs-env";
    buildInputs = with self; [ microhs emscripten nodejs quickjs ];
    # https://github.com/NixOS/nixpkgs/issues/139943#issuecomment-930432045
    # dmj: Fix for using emcc in Darwin shell environment
    #
    # dmj: to compile /only/ JS, and work w/ QuickJS:
    # $ emcc -sENVIRONMENT=shell -sWASM=0 main.c -oout.js && qjs out.js
    shellHook = with super; ''
      export MHSDIR=${self.microhs}
      export CC=${super.emscripten}/bin/emcc
      export PATH=$PATH:${self.microhs}/bin
      mkdir -p ~/.emscripten_cache
      chmod u+rwX -R ~/.emscripten_cache
      cp -r ${super.emscripten}/share/emscripten/cache ~/.emscripten_cache
      export EM_CACHE=~/.emscripten_cache
    '';
  };

}
