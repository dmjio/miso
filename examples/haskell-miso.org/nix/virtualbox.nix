{ resources, ... }:
{
  require = [ ./deployment.nix ];
  webserver.deployment.targetEnv = "virtualbox";
}
