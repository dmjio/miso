with (import ./default.nix);
dev.env.overrideAttrs (old: {
  shellHook = ''
    function reload () {
      ${pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c '${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl' -T ':main'
    }
  '';
})

