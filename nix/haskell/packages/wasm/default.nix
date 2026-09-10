pkgs:
let
  source = import ../../../source.nix pkgs;
in
with pkgs.haskell.lib;
self: super:
{
  /* miso */
  miso = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell" {};
  miso-tests = self.callCabal2nix "miso-tests" source.miso-tests {};

  # Miso.JSON via Data.Aeson instead of the built-in decoder, and (aeson-text)
  # MisoString backed by Data.Text -- mirrors `tests/Makefile`'s
  # --constraint="miso +aeson[+text]" builds.
  miso-aeson = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell -faeson" {};
  miso-aeson-text = self.callCabal2nixWithOptions "miso" source.miso "-ftemplate-haskell -faeson -ftext" {};
  miso-tests-aeson = self.callCabal2nix "miso-tests" source.miso-tests { miso = self.miso-aeson; };
  miso-tests-aeson-text = self.callCabal2nix "miso-tests" source.miso-tests { miso = self.miso-aeson-text; };

  /* examples */
  sample-app = self.callCabal2nix "app" source.sample-app {};
}
