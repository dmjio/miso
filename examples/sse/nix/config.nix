{ pkgs, config, ... }:
{
  imports = [ ./module.nix ];
  nixpkgs.config.packageOverrides = pkgs: {
    sse-haskell-miso = import ./../default.nix {};
  };
  services = {
    sse-haskell-miso.enable = true;
    nginx = {
      enable = true;
      virtualHosts = {
        "sse.haskell-miso.org" = {
           forceSSL = true;
           enableACME = true;
           locations = {
	     "/" = {
	        proxyPass = "http://localhost:3003";
            };
          };
        };
      };
    };
  };
}
