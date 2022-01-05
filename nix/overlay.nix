options: self: super: {
  darwin = super.darwin // {
    xcode = super.darwin.xcode.overrideAttrs (drv: {
      outputHash = "ec9f78b948abe341000d31f21b66051741b71163d778702b3e2e01659d60e3d2";
    });
  };
  pkgsCross = super.pkgsCross // {
    iphone64 = super.pkgsCross.iphone64 // {
      haskell = super.pkgsCross.iphone64.haskell // {
        packages = super.pkgsCross.iphone64.haskell.packages // {
          integer-simple = super.pkgsCross.iphone64.haskell.packages.integer-simple // {
            ghc8107 = super.pkgsCross.iphone64.haskell.packages.integer-simple.ghc8107.override {
              overrides = import ./haskell/packages/ghcARM self;
            };
          };
        };
      };
    };
  };
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc8107 = super.haskell.packages.ghc8107.override {
        overrides = import ./haskell/packages/ghc8107 self;
      };
      # ghc864 = super.haskell.packages.ghc864.override {
      #   overrides = selfGhc864: superGhc864: with super.haskell.lib; {
      #     happy = dontCheck (selfGhc864.callHackage "happy" "1.19.9" {});
      #     mkDerivation = args: superGhc864.mkDerivation (args // {
      #       enableLibraryProfiling = false;
      #       doCheck = false;
      #       doHaddock = false;
      #     });
      #   };
      # };
      ghcjs = super.haskell.packages.ghcjs.override {
        overrides = import ./haskell/packages/ghcjs options self;
      };
    };
  };
  deploy = rev: super.writeScript "deploy" ''
    export PATH=$PATH:${self.nixopsUnstable}/bin
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
    nixops modify examples/haskell-miso.org/nix/aws.nix -d haskell-miso \
      -Inixpkgs=https://github.com/nixos/nixpkgs/archive/6d1a044fc9ff3cc96fca5fa3ba9c158522bbf2a5.tar.gz
    nixops deploy -j1 -d haskell-miso --option substituters "https://cache.nixos.org/"
  '';
  more-examples = with super.haskell.lib; {
    inherit (self.haskell.packages.ghcjs) flatris the2048 snake miso-plane;
  };
}
