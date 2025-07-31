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
     };
   };
}
