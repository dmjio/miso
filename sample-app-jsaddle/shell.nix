with (import ./default.nix);
let
  reload-script = pkgs.writeScriptBin "reload" ''
      ${pkgs.haskell.packages.ghc865.ghcid}/bin/ghcid -c \
        '${pkgs.haskell.packages.ghc865.cabal-install}/bin/cabal new-repl' \
        -T ':main'
'';
in dev.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [reload-script];
})
