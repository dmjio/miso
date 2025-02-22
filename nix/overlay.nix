options: self: super: {

  nixosPkgsSrc =
    "https://github.com/nixos/nixpkgs/archive/6d1a044fc9ff3cc96fca5fa3ba9c158522bbf2a5.tar.gz";

  haskell-miso-org-test = self.nixosTest {
    nodes.machine = { config, pkgs, ... }: {
      imports = [ ../haskell-miso.org/nix/machine.nix ];
    };
    testScript = {nodes, ...}: with nodes;
      ''
      startAll;
      $machine->waitForUnit("haskell-miso.service");
      $machine->succeed("curl localhost:3002");
      '';
  };

  coverage = import ../tests {};

  sample-app-tagged =
    import ../sample-app {};

  sample-app-jsaddle-tagged-release =
    (import ../sample-app-jsaddle {}).release;

  sample-app-jsaddle-tagged-dev =
    (import ../sample-app-jsaddle {}).dev;

  inherit (import ../haskell-miso.org {})
    haskell-miso-client
    haskell-miso-server
    haskell-miso-runner;

  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc865 = super.haskell.packages.ghc865.override {
        overrides = import ./haskell/packages/ghc self;
      };
      ghc864 = super.haskell.packages.ghc864.override {
        overrides = selfGhc864: superGhc864: with super.haskell.lib; {
          happy = dontCheck (selfGhc864.callHackage "happy" "1.19.9" {});
          mkDerivation = args: superGhc864.mkDerivation (args // {
            enableLibraryProfiling = false;
            doCheck = false;
            doHaddock = false;
          });
        };
      };
      ghcjs86 = super.haskell.packages.ghcjs86.override {
        overrides = import ./haskell/packages/ghcjs options self;
      };
    };
  };
  deploy = rev: super.writeScript "deploy" ''
    export PATH=$PATH:${self.nixops}/bin
    export PATH=$PATH:${self.jq}/bin
    rm -rf ~/.nixops
    mkdir -p ~/.aws
    echo "[dev]" >> ~/.aws/credentials
    echo "aws_access_key_id = $AWS_ACCESS_KEY_ID" >> ~/.aws/credentials
    echo "aws_secret_access_key = $AWS_SECRET_ACCESS_KEY" >> ~/.aws/credentials
    mkdir -p ~/.ssh
    echo "Host *" > ~/.ssh/config
    echo "  StrictHostKeyChecking=no" >> ~/.ssh/config
    chmod 600 ~/.ssh/config
    chown $USER ~/.ssh/config
    echo $DEPLOY | jq > deploy.json
    nixops import < deploy.json
    rm deploy.json
    nixops set-args --argstr email $EMAIL -d haskell-miso
    nixops modify haskell-miso.org/nix/machine.nix -d haskell-miso -Inixpkgs=${self.nixosPkgsSrc}
    nix --version
    # https://github.com/NixOS/nixops/issues/1557
    nix shell github:nixos/nixpkgs/8ad5e8132c5dcf977e308e7bf5517cc6cc0bf7d8#nix -c \
      nixops deploy -j1 -d haskell-miso --option substituters "https://cache.nixos.org/"
  '';
  more-examples = with super.haskell.lib; {
    inherit (self.haskell.packages.ghcjs) flatris the2048 snake miso-plane;
  };
}
