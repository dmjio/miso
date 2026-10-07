{ pkg ? "ghc" }:

with (import ./default.nix {});

if pkg == "ghcjs9141"
then miso-ghcjs-9141.env.overrideAttrs (drv: {
  shellHook = ''
    export CC=${pkgs.emscripten}/bin/emcc
    mkdir -p ~/.emscripten_cache
    chmod u+rwX -R ~/.emscripten_cache
    cp -r ${pkgs.emscripten}/share/emscripten/cache ~/.emscripten_cache
    export EM_CACHE=~/.emscripten_cache
  '';
})
else miso-ghc-9141.env
