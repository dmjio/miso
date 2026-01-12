{ pkg ? "ghc" }:

with (import ../default.nix {});

if pkg == "ghcjs"
then miso-tests.env.overrideAttrs (drv: {
  shellHook = ''
    export CC=${pkgs.emscripten}/bin/emcc
    mkdir -p ~/.emscripten_cache
    chmod u+rwX -R ~/.emscripten_cache
    cp -r ${pkgs.emscripten}/share/emscripten/cache ~/.emscripten_cache
    export EM_CACHE=~/.emscripten_cache
    function build-tests () {
      ../scripts/test-runner.sh
    }
  '';
})
else
if pkg == "ghcjs"
then miso-tests.env
else miso-tests-ghc.env
