with (import ../../default.nix {});
{ config
, lib
, ...
}:
{
   services.nginx = {
     enable = true;
     recommendedGzipSettings = true;
     recommendedOptimisation = true;
     virtualHosts = {
       "haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
             proxyPass = "http://127.0.0.1:3002";
           };
         };
       };
       "todo-mvc.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/todo-mvc.jsexe";
           };
         };
       };
       "coverage.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.coverage}/lcov-report";
           };
         };
       };
       "haddocks.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-ghcjs.doc}/share/doc/miso-1.8.7.0/html";
           };
         };
       };
       "flatris.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.more-examples.flatris}/bin/app.jsexe";
           };
         };
       };
       "miso-plane.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = pkgs.more-examples.miso-plane;
           };
         };
       };
       "2048.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = pkgs.more-examples.the2048;
           };
         };
       };
       "threejs.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/threejs.jsexe";
           };
         };
       };
       "snake.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.more-examples.snake}/bin/app.jsexe";
           };
         };
       };
       "router.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/router.jsexe";
           };
         };
       };
       "mario.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/mario.jsexe";
           };
         };
       };
       "simple.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/simple.jsexe";
           };
         };
       };
       "canvas.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/canvas2d.jsexe";
           };
         };
       };
       "svg.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/svg.jsexe";
           };
         };
       };
       "file-reader.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/file-reader.jsexe";
           };
         };
       };
       "xhr.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/xhr.jsexe";
           };
         };
       };
       "websocket.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/websocket.jsexe";
           };
         };
       };
     };
   };
}
