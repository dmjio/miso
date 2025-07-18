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
       "components.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/components.jsexe";
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
            root = coverage;
           };
         };
       };
       "haddocks.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = haddocks;
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
       "fetch.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${miso-examples}/bin/fetch.jsexe";
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
