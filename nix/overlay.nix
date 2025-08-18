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
      cc = "cc"
      ccflags = "-w -Wall -O3"
      cclibs = "-lm"
      conf = "unix"

      [debug]
      cc = "cc"
      ccflags = "-w -Wall -g"
      cclibs = "-lm"
      conf = "unix"

      [emscripten]
      cc = "emcc"
      ccflags = "-O3 -sEXPORTED_RUNTIME_METHODS=stringToNewUTF8 -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sNODERAWFS -sSINGLE_FILE -DUSE_SYSTEM_RAW -sEXIT_RUNTIME -Wno-address-of-packed-member -sERROR_ON_UNDEFINED_SYMBOLS=0 -sWASM=0"
      cclibs = "-lm"
      conf = "unix"

      [tcc]
      cc = "tcc"
      ccflags = "-D__TCC__=1"
      cclibs = "-lm"
      conf = "unix"

      [windows]
      cc = "cl"
      ccflags = "-O2"
      cclibs = ""
      conf = "windows"
      cout = "-Fe"

      [environment]
      -- Get all values from the environment
      cc = "$CC"
      ccflags = "$MHSCCFLAGS"
      cclibs = "$MHSCCLIBS"
      conf = "$MHSCONF"
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
        ls -lah
        ls -lah $out/
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
