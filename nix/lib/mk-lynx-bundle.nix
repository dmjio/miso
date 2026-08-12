# mkLynxBundle — build a Lynx `.lynx.bundle` from a GHC-JS-backend app.
#
# Factored out of the in-tree `sample-app-native-bundle` so any downstream flake
# can reuse it. Given the app's ghcjs derivation (whose `bin/<exe>.jsexe/all.js`
# is the compiled program), plus an optional stylesheet and assets directory, it
# minifies all.js, wires up a small rspeedy entry, and runs `rspeedy build`.
#
# `styles.css` is compiled into the bundle under the global cssId 0 (Lynx compiles
# CSS ahead of time; runtime injection doesn't work), and every PNG under `assets`
# is `import`ed with `?inline` so rspeedy embeds it as a `data:` URI *in the
# bundle* (offline, no asset server) and registers it by relative name on
# `globalThis.__galleryAssets` for Haskell to read back.
#
# Usage (from a flake that has miso as an input):
#
#     miso.lib.${system}.mkLynxBundle {
#       name    = "my-app-bundle";
#       jsDrv   = myGhcjsApp;          # e.g. ghcNative.callCabal2nix ...
#       exeName = "my-app";            # bin/my-app.jsexe/all.js
#       assets  = ./assets;            # optional
#       styles  = ./styles.css;        # optional
#     }
pkgs:

{ name ? "lynx-bundle"
, jsDrv                    # the app's ghcjs derivation
, exeName                  # executable name; its `.jsexe/all.js` is the program
, assets ? null            # optional path to a directory of PNGs to inline
, styles ? null            # optional path to a styles.css to compile in
}:

with pkgs.lib;

let
  entryJs = pkgs.writeText "entry.js" (
    optionalString (assets != null) "import './__assets.js';\n"
    + optionalString (styles != null) "import './styles.css';\n"
    + "import './all.js';\n"
  );

  lynxConfig = pkgs.writeText "lynx.config.ts" ''
    import { defineConfig } from '@lynx-js/rspeedy';
    import { pluginReactLynx } from '@lynx-js/react-rsbuild-plugin';
    export default defineConfig({
      source: { entry: './entry.js' },
      plugins: [ pluginReactLynx() ],
    });
  '';
in
pkgs.stdenv.mkDerivation {
  inherit name;
  phases = [ "buildPhase" "installPhase" ];
  nativeBuildInputs = [ pkgs.bun pkgs.rspeedy pkgs.nodejs ];

  buildPhase = ''
    export HOME=$TMPDIR
    mkdir -p build

    # Pre-bundle all.js so rspeedy doesn't choke on the GHC JS RTS's Node shims.
    ${pkgs.bun}/bin/bun build \
      --minify-whitespace \
      --target=bun \
      --outfile=build/all.js \
      ${jsDrv}/bin/${exeName}.jsexe/all.js

    ln -s ${pkgs.rspeedy}/lib/node_modules build/node_modules
    cp ${lynxConfig} build/lynx.config.ts
    cp ${entryJs} build/entry.js
    ${optionalString (styles != null) "cp ${styles} build/styles.css"}
    ${optionalString (assets != null) ''
      cp -r ${assets} build/assets
      chmod -R u+w build/assets
      (
        cd build
        echo "const reg = {};"
        n=0
        for f in $(find assets -type f -name '*.png' | sort); do
          key="''${f#assets/}"
          echo "import a$n from './$f?inline';"
          echo "reg['$key'] = a$n;"
          n=$((n + 1))
        done
        echo "globalThis.__galleryAssets = reg;"
      ) > build/__assets.js
    ''}

    cd build
    ${pkgs.rspeedy}/bin/rspeedy build
  '';

  installPhase = ''
    mkdir -p $out
    cp -r dist/. $out/
    cd $out
    ${pkgs.coreutils}/bin/sha256sum main.lynx.bundle > main.lynx.bundle.sha256
  '';
}
