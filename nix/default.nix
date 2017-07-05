{ pulls ? ./test.json }:

let
  pkgs = import <nixpkgs>{};
in with (import ./lib.nix { inherit pkgs; });
with pkgs.lib;
let
  defaults = globalDefaults // {
    nixexprinput = "miso";
    nixexprpath = "release.nix";
    checkinterval = 60;
  };
  primary_jobsets = {
    miso-master = defaults // {
      description = "miso-master";
      inputs = {
        miso = mkFetchGithub "https://github.com/dmjio/miso master";
        nixpkgs = mkFetchGithub "https://github.com/nixos/nixpkgs-channels.git nixpkgs-unstable";
      };
    };
  };
  pr_data = builtins.fromJSON (builtins.readFile pulls);
  makePr = num: info: {
    name = "miso-${num}";
    value = defaults // {
      description = "PR ${num}: ${info.title}";
      inputs = {
        miso = mkFetchGithub "https://github.com/${info.head.repo.owner.login}/${info.head.repo.name}.git ${info.head.ref}";
        nixpkgs = mkFetchGithub "https://github.com/nixos/nixpkgs-channels.git nixpkgs-unstable";
      };
    };
  };
  pull_requests = listToAttrs (mapAttrsToList makePr pr_data);
  jobsetsAttrs = pull_requests // primary_jobsets;
in {
  jobsets = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);
}
