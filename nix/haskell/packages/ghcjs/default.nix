pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
with pkgs.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell" {};

  /* examples */
  sample-app-js = self.callCabal2nix "app" source.sample-app {};
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  servant-client-core = doJailbreak super.servant-client-core;
  servant-client-js = self.callCabal2nix "servant-client-js" source.servant-client-js {};
  miso-flatris = self.callCabal2nix "flatris" source.miso-flatris {};
  miso-plane-core = self.callCabal2nix "miso-plane" source.miso-plane {};
  miso-plane = pkgs.runCommand "miso-plane" {} ''
    mkdir -p $out
    cp -rv ${source.miso-plane}/public/images $out
    cp -v ${self.miso-plane-core}/bin/client.jsexe/* $out
    chmod +w $out/index.html
    cp -v ${source.miso-plane}/public/index.html $out
  '';
  miso-2048-core = self.callCabal2nix "hs2048" source.miso-2048 {};
  miso-2048 = pkgs.runCommand "hs2048" {} ''
    mkdir -p $out/bin
    cp -rv ${self.miso-2048-core}/bin/*.jsexe $out/*
    chmod +w $out/bin/*.jsexe
    chmod +w $out/bin/*.jsexe/index.html
    cp -v ${source.miso-2048}/static/main.css $out/bin/app.jsexe/main.css
    cp -v ${source.miso-2048}/static/index.html $out/bin/app.jsexe/index.html
  '';
  miso-snake = self.callCabal2nix "miso-snake" source.miso-snake {};
  miso-examples-core = self.callCabal2nix "miso-examples" source.examples {};
  miso-examples = pkgs.runCommand "miso-examples" {} ''
    mkdir -p $out/bin/mario.jsexe/imgs
    cp -fr ${self.miso-examples-core}/bin/*.jsexe $out/*
    cp -frv ${source.examples}/mario/imgs $out/bin/mario.jsexe/
    chmod +w $out/bin/todo-mvc.jsexe
  '';

  /* cruft */
  crypton = dontCheck super.crypton;
  cryptonite = dontCheck super.cryptonite;
  monad-logger = doJailbreak super.monad-logger;
  string-interpolate = doJailbreak super.string-interpolate;
  servant-server = doJailbreak super.servant-server;
}
