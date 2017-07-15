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
	        proxyPass = "http://localhost:3002";
            };
          };
        };
      };
    };
  };
}
