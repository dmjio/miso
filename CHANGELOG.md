# Changelog

All notable changes to `miso` are documented here.

## 1.13.0.0

### Added

- **Native mobile backend.** `miso` can now target native mobile devices by
  driving the [Lynx](https://lynxjs.org) dual-thread runtime instead of the
  browser DOM. New `Miso.Native` entry point (`native` / `nativeWithContext`),
  the `Miso.Native.Element.*` element / event / property / method vocabulary,
  and main-thread event handlers for low-latency gestures. Gated behind the
  `native` cabal flag (`-fnative`); web / WASM builds are unaffected.
- **App-global `context`.** A single value shared by every `Component` in the
  tree (miso's analogue of React Context): seed with `startAppWithContext`,
  read with `getContext` (or the first argument to `view`), update with
  `modifyContext` / `putContext`, and opt components into context-driven
  re-renders with `useContext`.

### Changed

- **Breaking: `View` and `Attribute` gained a `model` type parameter**
  (`View context model action`, `Attribute model action`). This lets event
  handlers read the current `model` and supports the native dual-thread
  `static` handler protocol. `VNode` now carries a `DirectEvents` set, the
  key moved into `SomeComponent (Maybe Key) …`, and `VComp` / `VCompStatic` /
  `SomeStaticComponent` were restructured. Downstream `view` and attribute
  signatures must be updated accordingly.
