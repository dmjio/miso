# Packages a wasmPkgs-built executable into a browser-loadable bundle --
# the wasm32-wasi analogue of GHCJS's <exe>.jsexe/ directory, which GHC's
# wasm backend has no equivalent of (it just emits a raw .wasm). Produces
# <exeName>.wasmexe/ containing the .wasm, the ghc_wasm_jsffi.js glue
# (via post-link.mjs), and an index.html/index.js harness using
# @bjorn3/browser_wasi_shim (from the CDN -- see browser-shim.nix).
pkgs:
let
  shim = import ./browser-shim.nix pkgs;
  ghcWasmMeta = (builtins.getFlake "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org").outputs.packages.${pkgs.stdenv.hostPlatform.system};
in
{ name, drv, exeName, title ? exeName, scripts ? "" }:
pkgs.stdenvNoCC.mkDerivation {
  inherit name;
  dontUnpack = true;
  nativeBuildInputs = [ ghcWasmMeta.all_9_14 ];
  buildCommand = ''
    mkdir -p $out/${exeName}.wasmexe
    cp ${drv}/bin/${exeName}.wasm $out/${exeName}.wasmexe/

    $(wasm32-wasi-ghc --print-libdir)/post-link.mjs \
      --input $out/${exeName}.wasmexe/${exeName}.wasm \
      --output $out/${exeName}.wasmexe/ghc_wasm_jsffi.js

    cp ${shim.indexJs exeName} $out/${exeName}.wasmexe/index.js
    cp ${shim.indexHtml title exeName scripts} $out/${exeName}.wasmexe/index.html
  '';
}
