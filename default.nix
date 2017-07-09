{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) runCommand;
  result = import ./miso.nix {};
in
  runCommand "miso" result ''
     mkdir -p $out/{lib,doc,examples}
     cp -r ${result.miso-ghcjs}/bin/* $out/examples
     cp -r ${result.miso-ghcjs}/lib/* $out/lib
     cp -r ${result.miso-ghcjs}/share/doc/* $out/doc/
     cp -r ${result.miso-ghc}/share/doc/* $out/doc/
   ''
