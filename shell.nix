{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs9122"
then miso-ghcjs-9122.env.overrideAttrs (drv: {
  shellHook = ''
    export NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1
    export CC=${pkgs.emscripten}/bin/emcc
    mkdir -p ~/.emscripten_cache
    chmod u+rwX -R ~/.emscripten_cache
    cp -r ${pkgs.emscripten}/share/emscripten/cache ~/.emscripten_cache
    export EM_CACHE=~/.emscripten_cache
  '';
})
else
if pkg == "ghcjs"
then miso-ghcjs.env
else miso-ghc-9122.env
