with (import ../default.nix {});
{
  inherit pkgs;
  sample-app-js = sample-app-js-9141;
  sample-app = sample-app-ghc9141;
}
