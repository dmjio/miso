pkgs:
let
  source = import ../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
#  jsaddle = self.callCabal2nix "jsaddle" "${source.jsaddle}/jsaddle" {};
#  jsaddle-dom = self.callCabal2nix "jsaddle-dom" source.jsaddle-dom {};
  miso = self.callCabal2nix "miso" source.miso {};
  miso-jsaddle = self.callCabal2nixWithOptions "miso" source.miso "-fjsaddle" {};
  miso-examples-jsaddle =
    self.callCabal2nixWithOptions "miso-examples" source.examples "-fjsaddle" { miso = self.miso-jsaddle; };
#  jsaddle-warp = dontCheck (self.callCabal2nix "jsaddle-warp" "${source.jsaddle}/jsaddle-warp" {});
#  webkit2gtk3-javascriptcore = self.callCabal2nix "webkit2gtk3-javascriptcore" source.webkit2gtk3-javascriptcore {};
  jsaddle-wkwebview = dontCheck (overrideCabal (self.callCabal2nix "jsaddle-wkwebview" "${source.jsaddle}/jsaddle-wkwebview" {}) (drv: {
    libraryFrameworkDepends =
      (drv.libraryFrameworkDepends or []) ++
      (if pkgs.stdenv.hostPlatform.useiOSPrebuilt
       then [ "${pkgs.buildPackages.darwin.xcode}/Contents/Developer/Platforms/${pkgs.stdenv.hostPlatform.xcodePlatform}.platform/Developer/SDKs/${pkgs.stdenv.hostPlatform.xcodePlatform}.sdk/System" ]
       else (with pkgs.buildPackages.darwin.apple_sdk.frameworks; [ Cocoa WebKit  ]));
    buildDepends = pkgs.lib.optional (!pkgs.stdenv.hostPlatform.useiOSPrebuilt) [ pkgs.buildPackages.darwin.cf-private ];
  }));
#  jsaddle-webkit2gtk = self.callCabal2nix "jsaddle-webkit2gtk" "${source.jsaddle}/jsaddle-webkit2gtk" {};
#  ghcjs-dom-jsaddle = self.callCabal2nix "ghcjs-dom-jsaddle" "${source.ghcjs-dom}/ghcjs-dom-jsaddle" {};
#  ghcjs-dom-jsffi = self.callCabal2nix "ghcjs-dom-jsffi" "${source.ghcjs-dom}/ghcjs-dom-jsffi" {};
#  ghcjs-dom = self.callCabal2nix "ghcjs-dom" "${source.ghcjs-dom}/ghcjs-dom" {};
}
