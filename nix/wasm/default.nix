self: super:
{

  # Pinned by flake.lock -- see nix/ghc-wasm-meta.nix (via nix/overlay.nix).
  ghc-wasm-meta =
    self.ghcWasmMeta.packages."${super.stdenv.hostPlatform.system}";

  wasm-ghc =
    self.ghc-wasm-meta.wasm32-wasi-ghc-9_14;

}
