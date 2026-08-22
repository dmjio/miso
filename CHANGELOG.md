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

  Because the flag is off by default, the `Miso.Native.*` modules do not
  appear in the Hackage-generated documentation — build locally with
  `-fnative`, or see the `sample-app-native` directory for a worked example
  with iOS and Android hosts.

- **App-global `context`.** A single value shared by every `Component` in the
  tree (miso's analogue of React Context): seed with `startAppWithContext`,
  read with `getContext` (or the first argument to `view`), update with
  `modifyContext` / `modifyContext_` / `putContext`, and opt components into
  context-driven re-renders with `useContext`. `ComponentInfo` gained a
  `componentInfoContext` lens. The motivating use case is propagating
  settings such as locale or theme to every component without threading them
  through `props`.

- **Cookie Store API.** New `Miso.Cookie` module wrapping the browser's
  [CookieStore API](https://developer.mozilla.org/en-US/docs/Web/API/CookieStore)
  as `Effect` combinators — `cookieGet`, `cookieGetAll`, `cookieSet`,
  `cookieDelete`, `cookieDeleteWith`, the `Cookie` record and `defaultCookie`
  constructor, plus `_`-suffixed synchronous variants. `Miso.Subscription.Cookie`
  adds `cookieChangeSub` for subscribing to `CookieChangeEvent`s. Requires a
  secure context (HTTPS or `localhost`); the error callback fires when the
  API is unavailable.

- **Synchronous `Miso.Fetch` variants.** `_`-suffixed counterparts for the
  whole surface — `getJSON_`, `postJSON_`, `postJSON'_`, `putJSON_`,
  `getText_`, `postText_`, `putText_`, `getBlob_`, `postBlob_`, `putBlob_`,
  `getFormData_`, `postFormData_`, `putFormData_`, `getUint8Array_`,
  `postUint8Array_`, `putUint8Array_`, `getArrayBuffer_`, `postArrayBuffer_`,
  `putArrayBuffer_`, `postImage_`, `putImage_`. Each blocks the calling
  thread and returns `Either (Response error) (Response body)`. Best used
  inside `Miso.Effect.io` / `io_` so the scheduler thread is not blocked.

- **Cross-thread effects.** `runOnBG` and `runOnMain` (with the supporting
  `Thread` type) dispatch an action's `update` onto the background (BTS) or
  main (MTS) thread of the Lynx runtime. Off the native runtime, or when
  already on the target thread, both behave as an ordinary `issue`.

- **Main-thread event handlers.** `onMain` / `onMainWithOptions` in
  `Miso.Event` register handlers that run directly on the main thread, for
  low-latency gesture and animation work. `Miso.Native.MainThread` provides
  `MainThreadRef` and the imperative operations those handlers drive.
  `eventHandlerConvert` / `eventHandlerDecoder` and the `EventHandler` type
  are exported for building custom handlers.

- **Static components.** `mountStatic` and `mountStaticWithProps` mount a
  `Component` through a `StaticPtr` (`SomeStaticComponent`), so the component
  survives the dual-thread boundary; `vcomp_` / `vcomp` turn the resulting
  pointer into a `View`. Unlike the non-static combinators these need no key —
  the compile-time `StaticKey` supplies identity. To opt a statically mounted
  child into `context` re-renders, set the field directly:
  `mountStatic comp { useContext = True }`. `mountUseContext` is the
  non-static equivalent.

- **Context-seeding SSR entry points.** `misoWithContext` and
  `prerenderWithContext` hydrate a server-rendered page with an explicit
  initial `context`; `setContext` seeds the global context for use from the
  `ToHtml` renderer. `Miso.Reload` gained matching `liveWithContext` and
  `reloadWithContext`.

- **Lynx thread detection.** `getThreads`, `onBTS` and `onMTS` in `Miso.FFI`
  report which thread the current code is executing on.

- **CSS helpers.** `transition_` builds a single shorthand `transition`
  declaration (so an imperative `transition: none` reset on the main thread
  clears it as one key), and `cubicBezier` produces a `cubic-bezier(…)`
  timing function.

- **`Miso.DSL` additions.** `await` for awaiting a JS promise from Haskell,
  and the `JSException` type (which now has an `Exception` instance).

- **`DirectEvents`.** `VNode` carries a set of directly-dispatched events,
  readable via `nodeDirectEvents`, used by the native runtime to skip the
  scratch-node round trip.

### Changed

- **Breaking: `View` and `Attribute` gained type parameters.**
  `View context model action` and `Attribute model action`. This lets event
  handlers read the current `model` and supports the native dual-thread
  `static` handler protocol. `VNode` now carries a `DirectEvents` set, the
  key moved into `SomeComponent (Maybe Key) …`, and `VComp` / `VCompStatic` /
  `SomeStaticComponent` were restructured. Downstream `view` and attribute
  signatures must be updated accordingly.

- **Breaking: `Miso.Binding` was removed.** The experimental lens-based
  parent/child model synchronisation mechanism (`Binding`, `Bindings`,
  `Precedence`, and the `bindings` field on `Component`) is gone, along with
  its propagation phase in the scheduler. Use the new app-global `context`
  for shared state, or asynchronous messaging via `broadcast` / `Miso.PubSub`
  for point-to-point communication.

- **Breaking: `parent` and `ROOT` were removed.** `Component` no longer
  carries a `parent`; the `ROOT` marker that demarcated the top of the page
  is unnecessary without it. Both are superseded by `context`.

- **Breaking: `Miso.Types.keyed` was removed.** Use the keyed constructors
  directly: `textKey` / `textKey_` for text, `fragment_` / `vfrag_` for
  fragments, `mount_` / `vcomp_` / `mountStatic` for components, and
  `key_` in the attribute list for element nodes.

- **Breaking: runtime internals dropped from `Miso.FFI`.** `mountComponent`,
  `unmountComponent` and `modelHydration` (and `getComponentContext` from
  `Miso.FFI.Internal`) were documented as runtime-use-only and have been
  removed as part of the dual-thread rework. They have no user-facing
  replacement.

- **Breaking: `autocomplete_` takes a `MisoString`.** It was
  `Bool -> Attribute action`, which could only produce `"on"` / `"off"` and
  could not express the many other valid values (`"email"`, `"new-password"`,
  …). It is now `MisoString -> Attribute action`; replace `autocomplete_ True`
  with `autocomplete_ "on"`.

- **Breaking: `Miso.Util.Parser.endOfInput` was generalised** from
  `Parser a ()` to `ParserT r [a] [] ()`. Call sites are unaffected unless
  they carried an explicit type annotation.

- **`MisoString` `length` and `take` are code-point based on WASM.** They
  previously counted UTF-16 code units, so a string holding a single
  astral-plane character (an emoji, say) reported a length of 2. They now
  agree with `Data.Text` and with the GHCJS backend. Only the WASM backend
  was affected.

- **`context` no longer requires `ToJSON` / `FromJSON`.** The constraints
  were unused — `context` is never sent across the dual-thread boundary.

### Fixed

- **`rAFSub` now cancels the pending animation frame on unsubscribe.**
  Release freed the `requestAnimationFrame` callback without cancelling the
  frame already queued in the browser; the next frame then invoked a freed
  callback and crashed the WASM RTS with `internal error: stg_ap_p_ret`.
  `Miso.Canvas`'s `draw` also moved from a `syncCallback` to an
  `asyncCallback`, fixing a `schedule: re-entered unsafely` crash when a
  component unmounted mid-diff.

- **`autocorrect_` and `spellcheck_` wrote to the wrong attribute.** Both
  emitted `autocomplete` instead of their own attribute name. `spellcheck_`
  additionally now emits `"true"` / `"false"` rather than `"on"` / `"off"`.

- **`MOUNT` errors on a missing `domRef`** instead of synthesizing a bogus
  parent node and failing later in the diff.

- **Key-based model recovery is gated on `liveMode`,** so a component no
  longer reuses an unrelated model outside of hot reload.

- **`pendingStaticKey` / `pendingMainThread` are reset before plain `On`
  handlers run,** preventing state from one handler leaking into the next.

- **`-fssr` compiles together with `-fnative`.**

- **`JSException` derives `Exception`,** so it can be `throw`n and `catch`ed
  normally.

### Performance

- **`StableName` dirty-checking extended to `context` and `props`.**
  `modelCheck` was generalised to `dirtyCheck :: Eq a => a -> a -> Bool` and
  applied to the remaining sites that performed a full structural `Eq` walk
  on every check. The common case — two reads of the same `IORef` returning
  the same heap object — now short-circuits on pointer equality, which
  matters most for large contexts such as i18n translation maps.

- **Main-thread events dispatch directly,** with no scratch-node or JS
  round trip.

- **The thread environment (`mts` / `bts` / `web`) is cached** as a static
  global in the runtime rather than re-queried on every `initialize` /
  `initComponent`.
