pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
with pkgs.lib;
self: super:
{
  /* miso */
  miso = doHaddock (self.callCabal2nix "miso" source.miso {});

  /* examples */
  sample-app-js = self.callCabal2nix "app" source.sample-app {};
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  flatris = self.callCabal2nix "flatris" source.flatris {};
  miso-plane-core = self.callCabal2nix "miso-plane" source.miso-plane {};
  miso-plane = pkgs.runCommand "miso-plane" {} ''
    mkdir -p $out
    cp -rv ${source.miso-plane}/public/images $out
    cp -v ${self.miso-plane-core}/bin/client.jsexe/* $out
    chmod +w $out/index.html
    cp -v ${source.miso-plane}/public/index.html $out
  '';
  the2048-core = self.callCabal2nix "hs2048" source.the2048 {};
  the2048 = pkgs.runCommand "hs2048" {} ''
    mkdir -p $out/bin
    cp -rv ${self.the2048-core}/bin/*.jsexe $out/*
    chmod +w $out/bin/*.jsexe
    chmod +w $out/bin/*.jsexe/index.html
    cp -v ${source.the2048}/static/main.css $out/bin/app.jsexe/main.css
    cp -v ${source.the2048}/static/index.html $out/bin/app.jsexe/index.html
  '';
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
}
