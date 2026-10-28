# The Haskell package set for the hls dev shell (nix develop .#hls): GHC 9.14.1
# with miso's overrides, plus what haskell-language-server needs on 9.14.
# Only the hls shell uses it, so miso's own builds are unaffected.
#
# The pinned nixpkgs doesn't build HLS for GHC 9.14 yet: 9.14's boot packages
# (base 4.22, containers 0.8, time 1.15) exceed the upper bounds of several of
# its dependencies. These are the overrides HLS itself uses for its GHC 9.14
# dev shell (devShells ghc914 in github:haskell/haskell-language-server
# flake.nix): lift those bounds, and leave out the plugins whose dependencies
# don't build on 9.14 (hlint, stan, ormolu, fourmolu). Drop them once the
# pinned nixpkgs supports HLS on 9.14.
pkgs:
let
  hlib = pkgs.haskell.lib;
in
pkgs.haskell.packages.ghc9141.override (old: {
  overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { })) (self: super: {
    # GHC 9.14.1's haddock hangs (idle, never finishes) on several of these
    # packages (attoparsec, megaparsec, scientific, ...); the shell doesn't
    # need their API docs.
    mkDerivation = args: super.mkDerivation (args // { doHaddock = false; });

    Cabal-syntax_3_14_2_0 = hlib.doJailbreak super.Cabal-syntax_3_14_2_0;
    Cabal_3_14_2_0 = hlib.disableLibraryProfiling (hlib.overrideCabal super.Cabal_3_14_2_0 (old: {
      postPatch = (old.postPatch or "") + ''
        sed -i 's/time\s*>= 1\.4\.0\.1\s*&& < 1\.15/time >=1.4.0.1/' Cabal.cabal
        sed -i 's/containers\s*>= 0\.5\.8\.0\s*&& < 0\.8/containers >=0.5.8.0/' Cabal.cabal
      '';
    }));
    algebraic-graphs = super.algebraic-graphs_0_8;
    haskell-language-server = pkgs.lib.pipe super.haskell-language-server [
      (hlib.compose.disableCabalFlag "hlint")
      (hlib.compose.overrideCabal (old: {
        buildDepends = builtins.filter
          (p: !(builtins.elem (p.pname or "") [ "refact" "apply-refact" "hlint" ]))
          (old.buildDepends or [ ]);
        libraryHaskellDepends = builtins.filter
          (p: !(builtins.elem (p.pname or "") [ "stan" "ormolu" "fourmolu" ]))
          (old.libraryHaskellDepends or [ ]);
        doCheck = false;
      }))
    ];
  } // builtins.listToAttrs (map (name: { inherit name; value = hlib.dontCheck (hlib.doJailbreak super.${name}); }) [
    "clay" "dec" "ghc-lib-parser" "ghc-trace-events" "hie-compat" "lucid"
    "singleton-bool" "http-api-data" "binary-instances" "lukko"
    "constraints-extras" "tasty-hspec" "tomland" "rebase" "dependent-map"
    "lsp-types" "lsp" "lsp-test" "ghcide" "HTTP" "relude" "generic-lens"
    "enummapset" "hiedb"
  ]));
})
