{ rev ? "a01a52a2d7e116e059d43d7803be313fb1a825ad"
, outputSha256 ? "0ps3cpaz46iffrb8xipzhdi64mpyhh2gfgp4bhbvg34lxv1q0xxi"
}:
with import (builtins.fetchTarball {
       url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
       sha256 = outputSha256;
     }) {};
with haskell.lib;
let
  haskellPkgs = haskell.packages.ghc843.override(oldAttrs: {
    overrides = self: super: {
      miso = super.callPackage ./miso-ghc-jsaddle.nix {};
      jsaddle = doJailbreak super.jsaddle;
      jsaddle-dom = doJailbreak (super.callPackage (
      fetchFromGitHub {
        owner = "ghcjs";
        repo = "jsaddle-dom";
        rev = "0c59032d9f584029b00a9427722d4e77a1ab9ee5";
        sha256 = "0p1l3y8hmqiaykabayazyx5fyv6ghsxxx9g47796bzw4jl71c8xw";
      }) {});
      jsaddle-warp =
        dontCheck
        ((super.callPackage (fetchFromGitHub {
          owner = "ghcjs";
          repo = "jsaddle";
          rev = "76d969d62c0c125bf58927224cac0448b429cd38";
          sha256 = "1fcw40w1x07daxwh4sbnf542v03p4858v8wbinsjw6vdabnm7aad";
        } + "/jsaddle-warp") {}).overrideAttrs(oldAttrs: {
          patchPhase = ''
            substituteInPlace jsaddle-warp.cabal --replace "aeson >=0.8.0.2 && <1.3" "aeson >=0.8.0.2 && <1.4";
          '';
          }));
    };
  });
in
{ miso = haskellPkgs.miso;
  miso-shell = haskellPkgs.shellFor { packages = p: [p.miso]; withHoogle = true; };
}
