# MicroHs (mhs) toolchain, the packages miso needs under mhs, and the sample
# app built with mhs for the browser (used by playwright-mhs in default.nix).
#
# Layout: the mhs installation (binaries, base package, runtime sources,
# targets.conf) lives in microhs/lib/mcabal like nixpkgs' microhs.  Each
# further derivation copies that package database, installs more packages
# into the copy with mcabal, and points mhs at it with -a.
self: super:
let
  src = import ../source.nix super;
  version = "0.16.6.0";
  # Package database of a derivation, as seen by mhs/mcabal.
  cabalDir = drv: "${drv}/lib/mcabal";
  mhsDir = drv: "${cabalDir drv}/mhs-${version}";
  # Start a writable copy of a package database in $out.
  copyDB = drv: ''
    mkdir -p $out/lib
    cp -r ${cabalDir drv} $out/lib/mcabal
    chmod -R u+w $out/lib/mcabal
    export CABALDIR=$out/lib/mcabal
    export HOME=$TMPDIR
  '';
  # mcabal with the package database of $CABALDIR
  mcabal = "mcabal -a$CABALDIR/mhs-${version}";
in
rec {
  microhs = super.stdenv.mkDerivation {
    pname = "microhs";
    inherit version;
    src = src.microhs;
    # The compiler is built from the pre-generated C (generated/mhs.c),
    # then 'make install' compiles it again with the installation paths,
    # and installs cpphs, mcabal and the base package.
    buildPhase = ''
      runHook preBuild
      make bin/mhs bin/cpphs bin/mcabal
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      export CABALDIR=$out/lib/mcabal
      export HOME=$TMPDIR
      make MCABAL=$out/lib/mcabal install
      mkdir -p $out/bin
      for b in mhs mcabal cpphs; do ln -s ../lib/mcabal/bin/$b $out/bin/$b; done
      runHook postInstall
    '';
    passthru = { isMhs = true; };
  };

  # ghc-compat, array, transformers, mtl, containers (and an empty
  # template-haskell, which containers depends on) installed for mhs.
  microhs-packages = super.stdenv.mkDerivation {
    pname = "microhs-packages";
    inherit version;
    dontUnpack = true;
    nativeBuildInputs = [ microhs ];
    installPhase = ''
      runHook preInstall
      ${copyDB microhs}
      inst() {
        rm -rf pkg; cp -r "$1" pkg; chmod -R u+w pkg
        (cd "pkg$2" && ${mcabal} -q install)
      }
      inst ${src.mhs-ghc-compat}
      inst ${src.mhs-array}
      inst ${src.mhs-transformers}
      # mtl 2.3.1: MicroHs cannot parse the poly-kinded instance head
      rm -rf pkg; cp -r ${src.mhs-mtl} pkg; chmod -R u+w pkg
      sed -i 's/^instance forall k (r :: k) (m :: (k -> Type)) \. MonadCont (ContT r m) where/instance MonadCont (ContT r m) where/' pkg/Control/Monad/Cont/Class.hs
      (cd pkg && ${mcabal} -q install)
      # containers depends on template-haskell (only for Lift instances, which are not compiled)
      rm -rf pkg; mkdir pkg
      cat > pkg/template-haskell.cabal <<CABAL
      cabal-version:      2.4
      name:               template-haskell
      version:            2.22.0.0
      synopsis:           Empty stand-in for template-haskell when building with MicroHs
      build-type:         Simple

      library
          default-language: Haskell2010
          build-depends:    base
      CABAL
      (cd pkg && ${mcabal} -q install)
      inst ${src.mhs-containers} /containers
      rm -rf pkg
      runHook postInstall
    '';
  };

  # miso built with mhs, installed as a package.
  miso-mhs = super.stdenv.mkDerivation {
    pname = "miso-mhs";
    version = "1.14.0.0";
    src = src.miso;
    nativeBuildInputs = [ microhs ];
    installPhase = ''
      runHook preInstall
      ${copyDB microhs-packages}
      # -text: MisoString is Data.JSString, as on the other client backends
      # (wasm, GHCJS).  Flip to "text" for MisoString = Data.Text; both build.
      ${mcabal} -q -f"-template-haskell -aeson -native -production -ssr -benchmark -text" install
      runHook postInstall
    '';
  };

  # The sample app built with mhs for the browser: index.html and a single
  # app.js (WebAssembly and miso.js embedded).
  sample-app-mhs-bundle = super.stdenv.mkDerivation {
    pname = "sample-app-mhs-bundle";
    version = "1.14.0.0";
    src = src.sample-app;
    nativeBuildInputs = [ microhs super.emscripten ];
    buildPhase = ''
      runHook preBuild
      export CABALDIR=$TMPDIR/mcabal
      cp -r ${cabalDir miso-mhs} $CABALDIR
      chmod -R u+w $CABALDIR
      export HOME=$TMPDIR
      # emcc needs a writable cache
      export EM_CACHE=$TMPDIR/emcache
      cp -r ${super.emscripten}/share/emscripten/cache $EM_CACHE
      chmod -R u+w $EM_CACHE
      ${mcabal} --options=-tbrowser build
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp static-mhs/index.html $out/
      cp dist-mcabal/bin/mhs/app $out/app.js
      runHook postInstall
    '';
  };

  # The integration tests (tests/app/Main.hs) built with mhs for the browser,
  # driven by ts/playwright.ts (see playwright-mhs in default.nix).
  miso-tests-mhs-bundle = super.stdenv.mkDerivation {
    pname = "miso-tests-mhs-bundle";
    version = "1.14.0.0";
    src = src.miso-tests;
    nativeBuildInputs = [ microhs super.emscripten ];
    buildPhase = ''
      runHook preBuild
      export CABALDIR=$TMPDIR/mcabal
      cp -r ${cabalDir miso-mhs} $CABALDIR
      chmod -R u+w $CABALDIR
      export HOME=$TMPDIR
      export EM_CACHE=$TMPDIR/emcache
      cp -r ${super.emscripten}/share/emscripten/cache $EM_CACHE
      chmod -R u+w $EM_CACHE
      ${mcabal} --options=-tbrowser build
      runHook postBuild
    '';
    installPhase = ''
      runHook preInstall
      mkdir -p $out
      cp ${../../sample-app/static-mhs/index.html} $out/index.html
      cp dist-mcabal/bin/mhs/component-tests $out/app.js
      runHook postInstall
    '';
  };
}
