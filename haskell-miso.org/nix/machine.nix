{ pkgs
, config
, lib
, ...
}:
{
   imports = [ ./module.nix ];
   services.haskell-miso.enable = true;
}
