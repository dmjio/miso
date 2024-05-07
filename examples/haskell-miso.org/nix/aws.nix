{ email ? ""
, token ? ""
}:
let
  accessKeyId = "dev";
  region = "us-east-2";
  awsBox = { pkgs, config, lib, resources, ... }: {
   imports = [ ./module.nix ];
   nixpkgs.config.packageOverrides = pkgs: {
     haskell-miso = import ./../default.nix {};
     misoPkgs = import ../../../default.nix { examples = true; };
     coverage = import ../../../tests {};
   };
   security.acme = {
     inherit email;
     acceptTerms = true;
   };
   services.haskell-miso.enable = true;
   services.nginx = {
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
       "todo-mvc.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/todo-mvc.jsexe";
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
            root = "${pkgs.misoPkgs.miso-ghcjs.doc}/share/doc/miso-1.8.4.0/html";
           };
         };
       };
       "flatris.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.pkgs.more-examples.flatris}/bin/app.jsexe";
           };
         };
       };
       "miso-plane.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = pkgs.misoPkgs.pkgs.more-examples.miso-plane;
           };
         };
       };
       "2048.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = pkgs.misoPkgs.pkgs.more-examples.the2048;
           };
         };
       };
       "threejs.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/threejs.jsexe";
           };
         };
       };
       "snake.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.pkgs.more-examples.snake}/bin/app.jsexe";
           };
         };
       };
       "router.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/router.jsexe";
           };
         };
       };
       "mario.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/mario.jsexe";
           };
         };
       };
       "simple.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/simple.jsexe";
           };
         };
       };
       "canvas.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/canvas2d.jsexe";
           };
         };
       };
       "svg.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/svg.jsexe";
           };
         };
       };
       "file-reader.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/file-reader.jsexe";
           };
         };
       };
       "xhr.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/xhr.jsexe";
           };
         };
       };
       "websocket.haskell-miso.org" = {
          forceSSL = true;
          enableACME = true;
          locations = {
          "/" = {
            root = "${pkgs.misoPkgs.miso-examples}/bin/websocket.jsexe";
           };
         };
       };
     };
   };
   boot.loader.grub.device = pkgs.lib.mkForce "/dev/nvme0n1";
   networking.firewall = {
     allowedTCPPorts = [ 80 22 443 ];
     enable = true;
     allowPing = true;
   };
   services.openssh.enable = true;
   deployment = {
      targetEnv = "ec2";
      ec2 = {
        ebsBoot = true;
        ebsInitialRootDiskSize = 40;
        securityGroups = [ resources.ec2SecurityGroups.miso-firewall ];
        elasticIPv4 = resources.elasticIPs.miso-ip;
        associatePublicIpAddress = true;
        inherit region accessKeyId;
        keyPair = resources.ec2KeyPairs.misoKeyPair;
        instanceType = "t3.small";
      };
    };
  };
in
{
  resources.ec2SecurityGroups.miso-firewall = {
    inherit accessKeyId region;
    rules = [
      { fromPort = 80; toPort = 80; sourceIp = "0.0.0.0/0"; }
      { fromPort = 22; toPort = 22; sourceIp = "0.0.0.0/0"; }
      { fromPort = 443; toPort = 443; sourceIp = "0.0.0.0/0"; }
    ];
  };

  resources.elasticIPs.miso-ip = {
    inherit region accessKeyId;
    vpc = true;
  };

  resources.ec2KeyPairs.misoKeyPair = {
    inherit region accessKeyId;
  };

  inherit awsBox;

  network.enableRollback = false;
  network.description = "Miso network";
}
