options: pkgs:
let
  source = import ../source.nix pkgs;
in
with pkgs.haskell.lib;
with pkgs.lib;
self: super:
{
  inherit (pkgs.haskell.packages.ghc8107) hpack;
#  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
#  jsaddle-dom = self.callCabal2nix "jsaddle-dom" source.jsaddle-dom {};
#  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${source.jsaddle}/jsaddle-warp" {});
#  jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" "${source.jsaddle}/jsaddle-webkit2gtk" {};
#  ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${source.ghcjs-dom}/ghcjs-dom-jsaddle" {};
#  ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${source.ghcjs-dom}/ghcjs-dom-jsffi" {};
#  ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${source.ghcjs-dom}/ghcjs-dom" {};
  flatris = self.callCabal2nix "hs-flatris" source.flatris {};
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

  the2048 = import source.the2048 { inherit pkgs; inherit (self) miso; };
  snake = self.callCabal2nix "miso-snake" source.snake {};
  mkDerivation = args: super.mkDerivation (args // { doCheck = false; });
  doctest = null;
  miso-examples = (self.callCabal2nixWithOptions "miso-examples" source.examples "-fjsaddle" {}).overrideDerivation (drv: {
    doHaddock = options.haddock;
    postInstall = ''
      mkdir -p $out/bin/mario.jsexe/imgs
      mkdir -p $out/bin/threejs.jsexe
      cp -r ${drv.src}/mario/imgs $out/bin/mario.jsexe/
      cp ${drv.src}/xhr/index.html $out/bin/xhr.jsexe/
      cp -fv ${drv.src}/three/index.html $out/bin/threejs.jsexe/
      ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
         --jscomp_off=checkVars \
         --externs=$out/bin/todo-mvc.jsexe/all.js.externs \
         $out/bin/todo-mvc.jsexe/all.js > temp.js
      mv temp.js $out/bin/todo-mvc.jsexe/all.js
      cp -fv ${drv.src}/todo-mvc/index.html $out/bin/todo-mvc.jsexe/
      cp -v ${source.todomvc-common}/base.css $out/bin/todo-mvc.jsexe
      cp -v ${source.todomvc-app-css}/index.css $out/bin/todo-mvc.jsexe
      '';
  });
  miso-jsaddle = self.callCabal2nixWithOptions "miso" source.miso "-fjsaddle" {};
  miso = (self.callCabal2nixWithOptions "miso" source.miso "-ftests" {}).overrideDerivation (drv: {
    doHaddock = options.haddock;
    postInstall = pkgs.lib.optionalString options.tests ''
      ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
         --jscomp_off=checkVars \
         --externs=$out/bin/tests.jsexe/all.js.externs \
           $out/bin/tests.jsexe/all.js > temp.js
           mv temp.js $out/bin/tests.jsexe/all.js
         '';
  });
}
