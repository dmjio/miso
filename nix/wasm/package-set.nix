# Builds a from-scratch, wasm32-wasi cross-compiled nixpkgs Haskell package
# set. Nixpkgs has no GHC of its own that targets wasm32-wasi, so this
# reaches for ghc-wasm-meta's prebuilt GHC/wasi-sdk toolchain instead, and
# patches nixpkgs' Haskell generic-builder (it assumes a target-specific
# lib dir layout the wasm backend doesn't use) so `callCabal2nix`/
# `mkDerivation` work against it.
#
# This is a separate, proper `haskell.packages`-shaped set (exposed as
# `wasmPkgs` from nix/overlay.nix) -- distinct from the ad-hoc single-file
# builder in ./default.nix, which predates this and has no cabal/hackage
# support. That one still backs the legacy nix/default.nix build (sampleWasm,
# wasm-ghc, ghc-wasm-meta) and is left untouched.
#
# Ported from https://github.com/ners/nix-wasm (default.nix and the two
# nixpkgs-patches/*.patch files).
{ pkgs, ghcWasmMeta, nixpkgsPath ? pkgs.path, system ? pkgs.stdenv.hostPlatform.system }:
let
  inherit (pkgs) lib;

  patchedNixpkgsPath = pkgs.stdenvNoCC.mkDerivation {
    name = "patched-nixpkgs";
    src = nixpkgsPath;
    patches = [
      ./nixpkgs-patches/generic-builder.patch
    ];
    dontBuild = true;
    installPhase = ''
      cp -r . "$out"
    '';
  };

  ghc = "ghc9141";
  targetPrefix = "wasm32-wasi-";
  ghcWasmMetaPkgs = ghcWasmMeta.packages.${system};
in
import patchedNixpkgsPath rec {
  inherit system;
  crossSystem = lib.systems.elaborate lib.systems.examples.wasi32 // {
    isStatic = false;
  };
  config.replaceCrossStdenv = { buildPackages, baseStdenv }: buildPackages.stdenvNoCC.override {
    inherit (baseStdenv)
      buildPlatform
      hostPlatform
      targetPlatform;
    cc = ghcWasmMetaPkgs.all_9_14 // {
      isGNU = false;
      isClang = true;
      libc = ghcWasmMetaPkgs.wasi-sdk.overrideAttrs (attrs: { pname = attrs.name; version = "unstable1"; });
      inherit targetPrefix;
      bintools = ghcWasmMetaPkgs.all_9_14 // {
        inherit targetPrefix;
        bintools = ghcWasmMetaPkgs.all_9_14 // {
          inherit targetPrefix;
        };
      };
    };
  };
  overlays = [
    (final: prev: {
      cabal-install = ghcWasmMetaPkgs.wasm32-wasi-cabal-9_14;
    })
  ];
  crossOverlays = [
    (final: prev: {
      cabal-install = ghcWasmMetaPkgs.wasm32-wasi-cabal-9_14;
      haskell = (prev.haskell.override (old: {
        buildPackages = lib.recursiveUpdate old.buildPackages {
          haskell.compiler.${ghc} = ghcWasmMetaPkgs.wasm32-wasi-ghc-9_14 // {
            inherit targetPrefix;
          };
        };
      })) // {
        packageOverrides = lib.composeManyExtensions [
          prev.haskell.packageOverrides
          (hfinal: hprev: {
            ghc = ghcWasmMetaPkgs.wasm32-wasi-ghc-9_14 // {
              inherit (pkgs.haskell.packages.${ghc}.ghc) version haskellCompilerName;
              inherit targetPrefix;
            };
            mkDerivation = args: (hprev.mkDerivation (args // {
              enableLibraryProfiling = false;
              enableSharedLibraries = true;
              enableStaticLibraries = false;
              # enableExternalInterpreter isn't a recognized mkDerivation
              # parameter at this nixpkgs pin (a newer addition than what
              # nix-wasm's snapshot had) -- generic-builder.nix's args are a
              # strict pattern, so passing it errors instead of being ignored.
              doBenchmark = false;
              doHaddock = false;
              doCheck = false;
              jailbreak = true;
              # (args.configureFlags or []) ++ ..., not a plain replacement --
              # callCabal2nixWithOptions-generated cabal flags (e.g.
              # -ftemplate-haskell) arrive via configureFlags too, and a
              # plain `configureFlags = [...]` here would silently discard them.
              configureFlags = (args.configureFlags or [ ]) ++ [
                "--with-ld=${prev.stdenv.cc.bintools}/bin/lld"
                "--with-ar=${prev.stdenv.cc.bintools}/bin/ar"
                "--with-strip=${prev.stdenv.cc.bintools}/bin/strip"
              ];
              setupHaskellDepends = (args.setupHaskellDepends or [ ]) ++ [
                # This executes the wasi-sdk setup-hook that sets toolchain env vars such as AR, CC, ...
                ghcWasmMetaPkgs.wasi-sdk
              ];
              preBuild = ''
                ${args.preBuild or ""}
                export NIX_CC=$CC
              '';
            })).overrideAttrs (attrs: {
              name = "${attrs.pname}-${targetPrefix}${attrs.version}";
              preSetupCompilerEnvironment = ''
                export CC_FOR_BUILD=$CC
              '';
            });
            zlib = prev.haskell.lib.compose.addBuildDepend hprev.zlib-clib hprev.zlib;
          })
          (import ../haskell/packages/wasm final)
        ];
      };
    })
  ];
}
