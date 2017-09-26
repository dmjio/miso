# In order to update `nixpkgs.json` to a specific revision, run:
#
# ```bash
# $ nix-prefetch-git https://github.com/NixOS/nixpkgs.git "${REVISION}" > nixpkgs.json
# ```

with rec {
  system = builtins.currentSystem;

  builtin-paths = import <nix/config.nix>;

  nixpkgs-json = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  # The nixpkgs to use when bootstrapping. This shouldn't matter except insofar
  # as it needs to have fetchFromGitHub and ideally the binary cache should be
  # populated for this.
  hashes = {
    commit = "788ce6e3df12bb0cf19fb9ccf8ffa75558b551ba";
    sha = "0ywvp5y6slff5xg1g63afhjywvqyz2pjxk3i203xph7fz3srpx2x";
  };

  stage1-tarball = import <nix/fetchurl.nix> {
    url = "https://github.com/NixOS/nixpkgs/archive/${hashes.commit}.tar.gz";
    sha256 = hashes.sha;
  };

  stage1-path = builtins.derivation {
    name = "nixpkgs-bootstrap-stage1";
    builder = builtin-paths.shell;
    args = [
      (builtins.toFile "nixpkgs-unpacker" ''
        "$coreutils/mkdir" -p "$out"
        cd "$out"
        "$gzip" -c -d "$tarball" > temporary.tar
        "$tar" -xf temporary.tar --strip-components=1
        "$coreutils/rm" temporary.tar
      '')
    ];

    inherit system;

    inherit (builtin-paths) tar gzip coreutils;
    tarball = stage1-tarball;
  };

  stage1 = import stage1-path { inherit system; };
};

stage1.fetchFromGitHub {
  owner = "NixOS";
  repo  = "nixpkgs";
  inherit (nixpkgs-json) rev sha256;
}
