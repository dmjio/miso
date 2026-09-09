self: super:
let
  js = import ./js super;
in
{
  # JS tooling
  inherit (js) rspeedy;

  # Reusable Lynx bundler (GHC-JS app -> .lynx.bundle). `self` is the final
  # overlayed pkgs, so it already has rspeedy/bun. See nix/lib/mk-lynx-bundle.nix.
  mkLynxBundle = import ./lib/mk-lynx-bundle.nix self;

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

  # A proper callCabal2nix-capable Haskell package set cross-compiled to
  # wasm32-wasi (nixpkgs has no GHC of its own that targets it, so this
  # reaches for ghc-wasm-meta's prebuilt toolchain -- see nix/wasm/package-set.nix).
  # e.g. wasmPkgs.haskell.packages.ghc9122.callCabal2nix (the real compiler
  # underneath is 9.14.1 -- see package-set.nix for why the slot is ghc9122).
  wasmPkgs = import ./wasm/package-set.nix {
    pkgs = super;
    ghcWasmMeta = (builtins.getFlake "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org").outputs;
  };

  # Packages a wasmPkgs-built executable into a browser-loadable bundle
  # (the wasm32-wasi analogue of mkLynxBundle above). See nix/wasm/mk-wasm-bundle.nix.
  wasmWebBundle = import ./wasm/mk-wasm-bundle.nix self;
}
