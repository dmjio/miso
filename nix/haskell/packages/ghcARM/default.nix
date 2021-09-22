pkgs:
let
  source = import ../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  mkDerivation = args: super.mkDerivation (args // {
    enableLibraryProfiling = false;
    doCheck = false;
    doHaddock = false;
  });
  ghc = super.ghc.overrideAttrs (drv: {
    patchPhase = ''
      sed -i -e '4092,4093d' compiler/main/DynFlags.hs
    '';
  });
  ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${source.ghcjs-dom}/ghcjs-dom-jsaddle" {};
  ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${src.ghcjs-dom}/ghcjs-dom-jsffi" {};
  ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${src.ghcjs-dom}/ghcjs-dom" {};
  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
  jsaddle-dom = self.callCabal2nix "jsaddle-dom" "${source.jsaddle}/jsaddle-dom" {};
  jsaddle-wkwebview = self.callCabal2nix "jsaddle-wkwebview" "${source.jsaddle}/jsaddle-wkwebview" {};
  servant = pkgs.lib.overrideDerivation (super.servant) (drv: {
    postInstall = "";
    postUnpack = ''
      ${pkgs.gnused}/bin/sed -i '135d' servant*/servant.cabal
      ${pkgs.gnused}/bin/sed -i '137d' servant*/servant.cabal
    '';
   });
  aeson = dontCheck super.aeson;
  QuickCheck = disableCabalFlag (super.QuickCheck) "templatehaskell";
  miso-examples-arm = self.callCabal2nix "miso-examples" source.examples {};
  miso = self.callCabal2nix "miso" source.miso  {};
}
