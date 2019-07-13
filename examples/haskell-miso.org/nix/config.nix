{ pkgs, config, ... }:
{
  imports = [ ./module.nix ];
  nixpkgs.config.packageOverrides = pkgs: {
    haskell-miso = import ./../default.nix {};
  };
  services = {
    haskell-miso.enable = true;
    nginx = {
      enable = true;
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
  };
}
