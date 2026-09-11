# `ghcWasmMeta` is the ghc-wasm-meta flake's outputs. flake.nix passes its
# locked input; everything else gets the same revision via nix/ghc-wasm-meta.nix,
# which reads it out of flake.lock.
{ ghcWasmMeta ? import ./ghc-wasm-meta.nix }:
self: super:
let
  js = import ./js super;
in
{
  # The wasm32-wasi toolchain everything below reads from. Exposed so the
  # legacy nix/wasm overlay and mk-wasm-bundle.nix share this one pin.
  inherit ghcWasmMeta;

  # JS tooling
  inherit (js) rspeedy;

  # Reusable Lynx bundler (GHC-JS app -> .lynx.bundle). `self` is the final
  # overlayed pkgs, so it already has rspeedy/bun. See nix/lib/mk-lynx-bundle.nix.
  mkLynxBundle = import ./lib/mk-lynx-bundle.nix self;

  # haskell stuff
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghcNative = super.haskell.packages.ghc9141.override {
        overrides = import ./haskell/packages/native self;
      };
      ghc9141 = super.haskell.packages.ghc9141.override {
        overrides = if super.stdenv.targetPlatform.isGhcjs
          then import ./haskell/packages/ghcjs self
          else import ./haskell/packages/ghc self;
      };
    };
  };

  # A proper callCabal2nix-capable Haskell package set cross-compiled to
  # wasm32-wasi (nixpkgs has no GHC of its own that targets it, so this
  # reaches for ghc-wasm-meta's prebuilt toolchain -- see nix/wasm/package-set.nix).
  # e.g. wasmPkgs.haskell.packages.ghc9141.callCabal2nix
  wasmPkgs = import ./wasm/package-set.nix {
    pkgs = super;
    inherit ghcWasmMeta;
  };

  # Packages a wasmPkgs-built executable into a browser-loadable bundle
  # (the wasm32-wasi analogue of mkLynxBundle above). See nix/wasm/mk-wasm-bundle.nix.
  wasmWebBundle = import ./wasm/mk-wasm-bundle.nix self;
}
