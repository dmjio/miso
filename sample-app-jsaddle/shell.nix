with (import ./default.nix);
let
  reload-script = pkgs.writeScriptBin "reload" ''
      ${pkgs.haskell.packages.ghc8107.ghcid}/bin/ghcid -c \
        '${pkgs.haskell.packages.ghc8107.cabal-install}/bin/cabal new-repl' \
        -T ':run Main.main'
'';
in dev.env.overrideAttrs (old: {
  buildInputs = old.buildInputs ++ [reload-script];
})
