{ compiler-nix-name="ghc982";
  crossPlatforms = p: [ p.ghcjs ];
  shell.tools.cabal = "latest";
}
