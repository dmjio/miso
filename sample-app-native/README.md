# Miso Native Lynx samples

This package builds two executables with different purposes:

- `app-native` is the feature gallery. Build it with
  `nix build .#sample-app-native-bundle`.
- `app-native-conformance` is a deterministic, core-elements-only dual-thread
  fixture. Build it with
  `nix build .#sample-app-native-conformance-bundle`.

The conformance fixture keeps stable element IDs for first paint, a BTS event
and declarative update, dynamic subtree removal/reinsertion, MTS-only style
mutation, MTS hydrated-state access, and nested component state. It stays
separate from the gallery so an unavailable optional native behavior cannot
truncate the tree before these runtime invariants execute.

Both bundle derivations produce `main.lynx.bundle` and
`main.lynx.bundle.sha256`. To run either with the Android host, copy both files
from the same derivation into `android/app/src/main/assets/`; the Gradle
`preBuild` gate rejects a missing or mismatched pair.

## Lynxtron (desktop)

`app-native-lynxtron` is a desktop sample for
[Lynxtron](https://github.com/lynx-family/lynxtron), an Electron-style host
that runs ordinary Lynx bundles in Node-managed windows. It exercises every
channel of `Miso.Native.Lynxtron` (request/reply, fire-and-forget, Node → card
events, and `contextBridge.exposeInLynxBTS` functions).

```bash
nix build .#sample-app-native-lynxtron-bundle     # => ./result/main.lynx.bundle
cd sample-app-native/desktop
npm install   # or: bun install — pulls @lynx-js/lynxtron + binary (macOS / Windows)
npm start     # or: bun run start — loads ../../result/main.lynx.bundle
```

`desktop/main.js` is the main process (`LynxWindow`, `lynxBridge.handle`,
`sendGlobalEvent`); `desktop/preload.js` exposes Node functions to the card.
Point `MISO_BUNDLE` at another bundle to host it instead.
