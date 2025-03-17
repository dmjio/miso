with (import ../default.nix {});
{
  inherit pkgs;
  inherit sample-app-js;
  inherit sample-app;
}
