pkgs:
# JS tooling for miso-native (LynxJS bundle building).
#
# `rspeedy` is the Lynx bundler CLI (`@lynx-js/rspeedy`, wrapping rspack). It is
# not in nixpkgs, so we install it from this repo's own `package.json` and
# `bun.lock` using bun2nix <https://github.com/nix-community/bun2nix>.
#
# Why bun2nix rather than `buildNpmPackage`: `buildNpmPackage` can only read an
# npm lockfile, which forced a second `package-lock.json` alongside `bun.lock`
# and a manual, unenforced "keep these in sync" rule. bun2nix consumes
# `bun.lock` directly, so there is one lockfile again.
#
# `nix/js/bun.nix` is GENERATED. Regenerate it whenever `bun.lock` changes:
#
#     bun run nix:lock          # or: nix run github:nix-community/bun2nix -- -o nix/js/bun.nix
#
# The bun2nix revision is pinned by the `bun2nix` input in `flake.nix` and read
# back out of `flake.lock` here, so both the flake and the legacy `nix-build`
# entry points use the same locked revision. Bump it with `nix flake update`.
let
  lock = builtins.fromJSON (builtins.readFile ../../flake.lock);

  node =
    lock.nodes.bun2nix.locked or (throw ''
      nix/js/default.nix: no `bun2nix` node in flake.lock.

      Add the input to flake.nix and run `nix flake lock` once:
          bun2nix.url = "github:nix-community/bun2nix";
    '');

  bun2nix =
    (builtins.getFlake "github:${node.owner}/${node.repo}/${node.rev}")
    .packages.${pkgs.stdenv.hostPlatform.system}.default;
in
{
  rspeedy = pkgs.stdenv.mkDerivation {
    pname = "rspeedy";
    version = "0.15.1";

    # Only the manifest and the lockfile matter; keep the rest of the repo out
    # so touching Haskell sources does not rebuild the toolchain.
    src = pkgs.lib.cleanSourceWith {
      src = ../..;
      filter = name: type:
        let base = builtins.baseNameOf name; in
        base == "package.json" || base == "bun.lock";
    };

    nativeBuildInputs = [
      bun2nix.hook
      pkgs.makeWrapper
      pkgs.nodejs
    ];

    bunDeps = bun2nix.fetchBunDeps {
      bunNix = ./bun.nix;

      # rspack ships its Rust binding as prebuilt, platform-specific `.node`
      # addons (@rspack/binding-<platform>), selected by the os/cpu fields
      # bun.lock records for each. Those are real Node addons built against V8,
      # so any dependency script that shells out to node must get *node*, not
      # bun's shim -- hence useFakeNode = false.
      useFakeNode = false;

      # The prebuilt bindings are ELF objects linked against the host libc and
      # libstdc++, which do not exist at those paths in the Nix store.
      autoPatchElf = pkgs.stdenv.hostPlatform.isLinux;
      nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
        pkgs.stdenv.cc.cc.lib
      ];
    };

    # We want rspeedy as a runnable tool, not to build this project.
    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p $out/bin $out/lib
      cp -r node_modules $out/lib/node_modules
      makeWrapper ${pkgs.nodejs}/bin/node $out/bin/rspeedy \
        --add-flags "$out/lib/node_modules/@lynx-js/rspeedy/bin/rspeedy.js"

      runHook postInstall
    '';

    meta = {
      description = "LynxJS rspeedy bundler CLI, installed from this repo's bun.lock";
      mainProgram = "rspeedy";
    };
  };
}
