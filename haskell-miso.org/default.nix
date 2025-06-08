{}:
with (import ../default.nix {});
let
  sources = import ../nix/source.nix pkgs;
  inherit (pkgs) runCommand;
  ghc = legacyPkgs.haskell.packages.ghc865;
  ghcjs = legacyPkgs.haskell.packages.ghcjs;
  client = ghcjs.callCabal2nix "haskell-miso" (sources.haskell-miso) {};
  server = ghc.callCabal2nix "haskell-miso" (sources.haskell-miso) {};
  dev = ghc.callCabal2nixWithOptions "haskell-miso" (sources.haskell-miso) "-fdev" {};
in
{ haskell-miso-client = client;
  haskell-miso-server = server;
  haskell-miso-dev = dev;
  haskell-miso-runner = 
    runCommand "haskell-miso.org" { inherit client server; } ''
      mkdir -p $out/{bin,static}
      cp ${server}/bin/* $out/bin
      cp -v ${client}/bin/client.jsexe/all.js $out/static/all.js
      cp -v ${miso-logos}/* $out/static/
    '';
}
