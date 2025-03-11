{ pkgs, config, ... }:
{
  imports = [ ./module.nix ];
  services = {
    sse-haskell-miso.enable = true;
    nginx = {
      enable = true;
      virtualHosts = {
        "sse.haskell-miso.org" = {
      	   extraConfig = "
      	     proxy_set_header Connection '';
      	     proxy_http_version 1.1;
      	     chunked_transfer_encoding off;
      	     proxy_buffering off;
      	     proxy_cache off;
      	   ";
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
