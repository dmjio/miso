pkgs:
let
  source = import ../source.nix pkgs;
in
with pkgs.haskell.lib;
with pkgs.lib;
self: super:
{
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
  the2048 = import source.the2048 { inherit pkgs; inherit (self) miso; };
  snake = self.callCabal2nix "miso-snake" source.snake {};
  miso-examples-core = self.callCabal2nix "miso-examples" source.examples {};
  miso-examples = pkgs.runCommand "miso-examples" {} ''
    mkdir -p $out/bin/mario.jsexe/imgs
    cp -fr ${self.miso-examples-core}/bin/*.jsexe $out/*
    cp -frv ${source.examples}/mario/imgs $out/bin/mario.jsexe/
    chmod +w $out/bin/xhr.jsexe/index.html
    cp -fv ${source.examples}/xhr/index.html $out/bin/xhr.jsexe/index.html
    chmod +w $out/bin/threejs.jsexe/index.html
    cp -fv ${source.examples}/three/index.html $out/bin/threejs.jsexe/index.html
    chmod +w $out/bin/todo-mvc.jsexe/index.html
    cp -fv ${source.examples}/todo-mvc/index.html $out/bin/todo-mvc.jsexe/index.html
    chmod +w $out/bin/todo-mvc.jsexe
    cp -fv ${source.todomvc-common}/base.css $out/bin/todo-mvc.jsexe/base.css
    cp -fv ${source.todomvc-app-css}/index.css $out/bin/todo-mvc.jsexe/index.css
  '';
  miso = (self.callCabal2nixWithOptions "miso" source.miso "-ftests" {})
    .overrideAttrs (drv: {
        doHaddock = true;
    });
}
