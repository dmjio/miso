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
      mkdir -p $out/report
      cp -rv $src $out/report
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
    };
  };

  microhs-wrapper = super.writeShellScriptBin "mhs" ''
    export MHSDIR=${self.microhs}
    exec "${self.microhs}/lib/bin/mhs" "$@"
  '';

  microhs =
    super.stdenv.mkDerivation {
      name = "MicroHs";
      src = (import ../nix/source.nix super).microhs;
      buildInputs = with super; [ emscripten ];
      installPhase = ''
        mkdir -p $out/{bin,share,lib/bin}

        cp -v ./bin/mcabal $out/bin
        cp -v ./bin/cpphs $out/bin
        cp -rv ./lib $out
        cp -v ./bin/mhs $out/lib/bin/mhs

        cp -rv ./generated $out
        cp -rv ./boards $out
        cp -rv ./paths $out
        cp -rv ./doc $out
        cp -rv ./src $out
        cat ./targets.conf

        cp README.md $out/share
      '';
    };

  microhs-env = super.mkShell {
    name = "microhs-env";
    buildInputs = with self;
      [ microhs microhs-wrapper emscripten nodejs quickjs
      ];
  };

}
