with (import ./default.nix);
dev.env.overrideAttrs (old: {
  shellHook = ''
    function reload () {
      ${pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -T ':main'
    }
  '';
})

