options:
with (builtins.fromJSON (builtins.readFile ./nixpkgs.json));
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  config = {
    allowUnfree = true;
    allowBroken = false;
  };
  overlays = [ (import ./overlay.nix options) ];
in
  import nixpkgs
    { inherit overlays config;
    }
