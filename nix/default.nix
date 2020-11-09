options:
with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  config.allowUnfree = options.allowUnfree;
  config.allowBroken = options.allowBroken;
  overlays = [ (import ./overlay.nix options) ] ++ options.overlays;
in
  import nixpkgs
    { inherit (options) crossSystem crossOverlays system;
      inherit overlays config;
    }
