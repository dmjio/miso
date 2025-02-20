{ pkg ? "ghcjs" }:
(import ./default.nix {})."miso-${pkg}".env
