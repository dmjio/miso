pkgs:
# JS tooling for miso-native (LynxJS bundle building).
#
# rspack ships platform-specific Rust-compiled .node binaries as optional npm
# dependencies (e.g. @rspack/binding-linux-x64-gnu). buildNpmPackage handles
# these by compiling the binding from source, so rustPlatform is required.
#
# To update npmDepsHash after changing package.json / package-lock.json:
#   nix build .#rspeedy 2>&1 | grep 'got:' | awk '{print $2}'
# or run: prefetch-npm-deps package-lock.json
#
# package-lock.json must be kept in sync with bun.lock.
# Generate it with: npm install --package-lock-only
let
  src = pkgs.lib.cleanSourceWith {
    src = ../..;
    filter = name: type:
      let base = builtins.baseNameOf name; in
      base == "package.json" || base == "package-lock.json";
  };
in
{
  rspeedy = pkgs.buildNpmPackage {
    pname = "rspeedy";
    version = "0.15.1";
    inherit src;

    # Run: prefetch-npm-deps package-lock.json to get this hash.
    # Use lib.fakeHash initially to let nix tell you the real value.
    npmDepsHash = "sha256-hh7duzVNIccp5tUsFfMc0fWb9pDgV9BimkqLgUy7Snk=";

    npmFlags = [ "--legacy-peer-deps" "--include=dev" ];
    makeCacheWritable = true;

    # We want rspeedy as a runnable tool, not to build this project.
    dontNpmBuild = true;

    installPhase = ''
      mkdir -p $out/bin $out/lib
      cp -r node_modules $out/lib/node_modules
      makeWrapper ${pkgs.nodejs}/bin/node $out/bin/rspeedy \
        --add-flags "$out/lib/node_modules/@lynx-js/rspeedy/bin/rspeedy.js"
    '';

    nativeBuildInputs = with pkgs; [
      rustc
      cargo
      nodejs
      makeWrapper
    ];
  };
}
