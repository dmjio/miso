{
  require = [ ./deployment.nix ];
  resources.sshKeyPairs.ssh-key = {};
  webserver = { resources, ...}: {
    nixpkgs.config.packageOverrides = pkgs: {
      haskell-miso = import ./../default.nix {};
    };
    services = {
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
      openssh.enable = true;
      haskell-miso.enable = true;
    };
    networking.firewall = {
      enable = false;
      allowPing = true;
      allowedTCPPorts = [ 443 80 22 8080 ];
    };
    deployment = {
      targetEnv = "digitalOcean";
      digitalOcean = {
        enableIpv6 = true;
        region = "nyc1";
        size = "1gb";
      };
    };
  };
}

