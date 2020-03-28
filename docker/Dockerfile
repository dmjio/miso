FROM lnl7/nix:2.3.3 

RUN nix-env -iA \
        nixpkgs.curl \
        nixpkgs.jq \
        nixpkgs.git \
        nixpkgs.gnutar \
        nixpkgs.gzip \
        # get ca certificates for connecting to cachix
        nixpkgs.libressl \
        # install ag and entr for auto-rebuild
        nixpkgs.silver-searcher \
        nixpkgs.entr \
        nixpkgs.cabal-install

RUN nix-env -iA cachix -f https://cachix.org/api/v1/install
RUN SYSTEM_CERTIFICATE_PATH=$NIX_SSL_CERT_FILE USER=miso cachix use miso-haskell

COPY ./sample-app /miso/sample-app 

WORKDIR /miso/sample-app
RUN nix-build