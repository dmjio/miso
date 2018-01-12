{ pulls ? ./test.json
, pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "a0aeb23";
    sha256 = "04dgg0f2839c1kvlhc45hcksmjzr8a22q1bgfnrx71935ilxl33d";
  }){}
}:
with (import ./lib.nix { inherit pkgs; });
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
        inherit pkgs;
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
        inherit pkgs;
      };
    };
  };
  pull_requests = listToAttrs (mapAttrsToList makePr pr_data);
  jobsetsAttrs = pull_requests // primary_jobsets;
in {
  jobsets = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);
}
