{
  description = "A flake for miso";

  nixConfig = {
    extra-substituters = [
      "https://haskell-miso-cachix.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-miso-cachix.cachix.org-1:m8hN1cvFMJtYib4tj+06xkKt5ABMSGfe8W7s40x1kQ0="
    ];
  };

  inputs = {
    nixpkgs.url =
      "https://github.com/alexfmpe/nixpkgs/archive/b594b289740a2bc917ed9c66fef5d905f389cb96.tar.gz";

    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
      ];

      perSystem = { config, self', inputs', pkgs, system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ (import ./nix/overlay.nix) ];
          config = {
            allowUnfree = true;
            allowBroken = false;
          };
        };

        packages.default = pkgs.haskell.packages.ghc9122.miso;

        devShells.default = pkgs.mkShell {
          name = "The miso ghc9122 shell";
          buildInputs = with pkgs; [
            pkgs.pkgsCross.ghcjs.haskell.packages.ghc9122.miso
          ];
        };
      };
    };
}
