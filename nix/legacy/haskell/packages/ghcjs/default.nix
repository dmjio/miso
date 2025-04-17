options: pkgs:
let
  source = import ../../../../source.nix pkgs;
in
with pkgs.haskell.lib;
with pkgs.lib;
self: super:
{
  inherit (pkgs.haskell.packages.ghc865) hpack;
  sample-app-js = self.callCabal2nix "app" source.sample-app {};
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${source.jsaddle}/jsaddle-warp" {});
  flatris = self.callCabal2nix "flatris" source.flatris {};
  miso-plane =
    let
      miso-plane = self.callCabal2nix "miso-plane" source.miso-plane {};
    in
      pkgs.runCommand "miso-plane" {} ''
         mkdir $out
         cp -rv ${source.miso-plane}/public/images $out
         cp ${miso-plane}/bin/client.jsexe/* $out
         rm $out/index.html
         cp -v ${source.miso-plane}/public/index.html $out
      '';
  hs2048 = import source.hs2048 { inherit pkgs; inherit (self) miso; };
  snake = self.callCabal2nix "miso-snake" source.snake {};
  mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
  doctest = null;
  miso-examples = (self.callCabal2nix "miso-examples" source.examples {}).overrideDerivation (drv: {
    postInstall = ''
      mkdir -p $out/bin/mario.jsexe/imgs
      mkdir -p $out/bin/threejs.jsexe
      cp -r ${drv.src}/mario/imgs $out/bin/mario.jsexe/
      cp -fv ${drv.src}/three/index.html $out/bin/threejs.jsexe/
      ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
         --jscomp_off=checkVars \
         --externs=$out/bin/todo-mvc.jsexe/all.js.externs \
         $out/bin/todo-mvc.jsexe/all.js > temp.js
      mv temp.js $out/bin/todo-mvc.jsexe/all.js
      '';
  });
  miso-prod = self.callCabal2nixWithOptions "miso" source.miso "-fproduction" {};
  miso = doHaddock (self.callCabal2nix "miso" source.miso {});
}
