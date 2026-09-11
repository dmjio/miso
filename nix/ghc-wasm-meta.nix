# The ghc-wasm-meta flake, pinned to the revision recorded in flake.lock.
#
# flake.nix receives it as a locked input; this is for everything evaluated
# outside the flake -- default.nix / nix-build, and the overlay's default --
# so both resolve the same toolchain and `nix flake update ghc-wasm-meta`
# bumps all of it at once. The rev + narHash make the reference locked, which
# is what lets `builtins.getFlake` run under pure evaluation: an unlocked
# reference errors there ("cannot call 'getFlake' on unlocked flake
# reference"), which broke `overlays.default` for downstream flakes.
let
  lock = builtins.fromJSON (builtins.readFile ../flake.lock);
  node = lock.nodes.${lock.nodes.root.inputs.ghc-wasm-meta}.locked;
  # A base64 narHash can contain '+', '/' and '='; percent-encode for the URL.
  encode = builtins.replaceStrings [ "+" "/" "=" ] [ "%2B" "%2F" "%3D" ];
in
(builtins.getFlake
  "${node.type}:${node.owner}/${node.repo}/${node.rev}?host=${node.host}&narHash=${encode node.narHash}"
).outputs
