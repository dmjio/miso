# MicroHs (mhs) toolchain and the packages miso depends on, installed for mhs.
#
# Layout: the mhs installation (binaries, base package, runtime sources,
# mhs.conf) lives in microhs/lib/mcabal like nixpkgs' microhs.  Each
# further derivation copies that package database, installs more packages
# into the copy with mcabal, and points mhs at it with -a.
#
# The attribute is named microhs, so in this overlay it replaces nixpkgs'
# microhs (upstream MicroHs) with dmjio/MicroHs (branch jsval), which adds
# JSVal, a JavaScript FFI and browser targets.
self: super:
let
  src = import ../source.nix super;
  version = "0.16.7.0";
  # Package database of a derivation, as seen by mhs/mcabal.
  cabalDir = drv: "${drv}/lib/mcabal";
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
}
