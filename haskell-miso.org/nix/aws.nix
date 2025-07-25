{ email ? ""
, token ? ""
}:
let
  accessKeyId = "dev";
  region = "us-east-2";
  awsBox =
    { pkgs, config, lib, resources, ...}: {
      imports =
        [ ./machine.nix
          ./nginx.nix
        ];
      security.acme = {
        inherit email;
        acceptTerms = true;
      };
      nix.gc.automatic = true;
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
