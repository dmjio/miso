# Changelog

All notable changes to `miso` are documented here.

## Unreleased

### Added

- **`VModel` / `vmodel` / `withModel`.** The `model` counterpart of
  `VProps`: an ambient accessor (not a node) wrapping a
  `model -> View context props model action` function, so a helper deep in a
  view tree can read the enclosing component's `model` without needing it
  threaded through as an explicit argument. Resolved against the mounting
  component's current `model` whenever the enclosing `View` is built, and
  against its initial (or hydrated) `model` when rendered with `toHtml`.
  `withModel` is a synonym for `vmodel`.
- **`svgWith_`** (`Miso.Native.X.Element.Svg`). Native inline SVG content
  that reads the enclosing component's `context` / `props` / `model`:
  `svgWith_ attrs content` is an `<svg>` that obtains all three ambiently via
  `withContext` / `withProps` / `withModel` and hands them to `content_`, so
  the content may use `vcontext` / `vprops` / `vmodel` directly.
- **`VProps` / `vprops` / `withProps`.** The `props` counterpart of
  `VContext`: an ambient accessor (not a node) wrapping a
  `props -> View context props model action` function, so a helper deep in a
  view tree can read the enclosing component's `props` without needing it
  threaded through as an explicit argument. `ImplicitParams` or a `Reader`
  could serve the same purpose, at the cost of a GHC-specific extension or a
  monadic style for view code. Resolved against the mounting
  component's current `props` whenever the enclosing `View` is built or
  rendered (`toHtml` on a bare `View` uses `()`). `withProps` is a synonym
  for `vprops`.
- **`toHtmlWith`.** `toHtmlWith :: context -> props -> model -> View context
  props model action -> ByteString` renders a `View` whose `context`, `props`
  or `model` type is not `()`, supplying the values `VContext` / `VProps` /
  `VModel` accessors resolve against.
- **`VContext` / `vcontext` / `withContext`.** An ambient accessor (not a
  node) wrapping a `context -> View context props model action` function, so a helper deep in a view tree can read the app-global
  `context` without needing it threaded through as an explicit argument.
  Resolved against the current `context` whenever the enclosing `View` is
  built or rendered; it does not itself trigger a redraw — that is still
  governed solely by `useContext`. `withContext` is a synonym for `vcontext`.

### Removed

- **Breaking: `setContext`.** It existed to seed the `globalContext` cell for
  the `ToHtml` renderer, and that cell is gone: the renderer now takes the
  `context` as an argument, `ToHtml` requires `context ~ ()`, and there is no
  cell at all until an app starts, so the call it was added for is now a type
  error rather than something you fix by calling it first. Seed the `context`
  with `startAppWithContext` / `misoWithContext` / `prerenderWithContext`,
  change it from `update` with `modifyContext` / `putContext`, and pass it to
  `toHtmlWith` for server-side rendering.

### Changed

- **Breaking: `view` takes only the `model`.** The `view` field of a
  `Component` was `context -> props -> model -> View context props model
  action`; it is now `model -> View context props model action`. The
  `context` and `props` are read where they are actually needed with the
  ambient accessors `withContext` / `vcontext` and `withProps` / `vprops`,
  which resolve at the point the enclosing `View` is built or rendered. A
  `view` that ignored them no longer has to name them (`\_ _ _ -> …` becomes
  `\_ -> …`), and one that used them reads them at the point of use instead
  of threading them down through every helper. `toHtmlWith` still takes the
  `context`, `props` and `model` — they are what the accessors in the
  rendered tree resolve against — but the `view` it is applied to now takes
  only the `model`: `toHtmlWith ctx props model (view comp model)`.

- **Breaking: the app-global `context` cell is owned by the app, not by a
  top-level CAF.** The `globalContext` `IORef` is gone. `initComponent` now
  creates the cell and every mounted component holds a *reference* to it as
  `_componentContext` (lens `componentContext`), so there is exactly one
  value per running app: no component holds a copy, none can disagree, and
  `modifyContext` (via the new `modifyContextAll`) is a single
  `atomicModifyIORef'` on that cell rather than a rewrite of every component.
  On Lynx each thread runs its own `initComponent` and so owns its own cell.
  `buildVTree` receives the context as an explicit argument, so `VContext` is
  resolved exactly like `VProps`, and the render path takes a plain value —
  no effectful read inside rendering, and no `undefined`-seeded global to
  force. The scheduler's propagation pass is unchanged. Consequences:
  `toHtmlWith` takes the `context`
  first (`toHtmlWith ctx props model view`) and `ToHtml (View …)` requires
  `context ~ ()` alongside `props ~ ()` and `model ~ ()`; the Lynx SVG
  `content_` takes the `context`, `props` and `model` ahead of the content
  `View`; `Miso.Reload`'s stable pointer no longer carries a context cell and
  recovers the old context by reading the root component's cell, seeding a
  fresh one on reload.
- **`SomeStaticComponent` now holds the component; `propsTypeOnly` is
  gone.** A static mount is the component itself bundled with its
  dictionaries, `SomeStaticComponent props context` (existential over `model`
  / `action`, `props` kept visible), rather than a `props -> SomeComponent`
  function. `VCompStatic` holds `StaticPtr (SomeStaticComponent props context)`
  next to the `props` value and the runtime builds the `SomeComponent` at
  mount time. Because the dictionaries now sit in the value a `StaticKey`
  resolves to, the Lynx main thread recovers the child's `action` and
  `props` types from the key alone, so the lazy `propsTypeOnly` placeholder
  and the "every constructor is lazy in props" invariant are gone.
  `mountStatic` now accepts components with or without `props`;
  `mountStaticWithProps` remains as a deprecated alias until 1.15. Code that
  pattern-matches the `SomeStaticComponent` constructor directly sees its
  payload change from a function to a `Component`. A new `MountConstraints`
  synonym declares the shared dictionary set once for `SomeComponent` and
  `SomeStaticComponent`. Call sites such as
  `vcomp_ (static (mountStatic comp))` are unchanged.
- **`toHtml` collapses adjacent text nodes inside fragments and across a
  `[View]`.** Previously only an element's direct children were collapsed,
  so an empty text node inside a `vfrag` rendered as a lone space that the
  client's hydration walk (which recurses into fragments) could not
  reconcile, silently falling back to a full re-render.
- **`vcontext` / `vprops` / `vmodel` children are resolved before being built.** The
  runtime now looks through these wrappers before deciding what to do with
  a child, so one that resolves to an empty fragment is skipped like an
  inline `fragment []`, and one that resolves to a plain node has its JS
  handle freed after linking like any other node. `toHtml` likewise
  resolves them before collapsing adjacent text nodes, matching the
  client's hydration-time tree.
- **Breaking: `View` gained a `props` type parameter.** `View context model
  action` is now `View context props model action`, matching the order of
  `Component context props model action`. Unlike `context`, `props` are
  per-component, so `VProps` is resolved through the type system the same
  way `model` is, rather than through a global cell. Mount combinators
  (`mount_`, `mountWithProps`, `(+>)`, `vcomp`, …) forget the child's
  `props` at the boundary exactly as they forget its `model` and `action`.
  Update signatures by inserting a `props` variable after `context`; code
  that leaves it polymorphic needs no other change. `ToHtml (View …)` and
  `ToHtml [View …]` now require `context ~ ()`, `props ~ ()` and
  `model ~ ()`: a bare `View` is static markup, and a component's view is
  rendered with `toHtmlWith` (so `toHtml (view ctx () m)` becomes
  `toHtmlWith ctx () m (view ctx () m)`). `content_` in
  `Miso.Native.X.Element.Svg.Property` now takes the `context`, `props` and
  `model` to render against, ahead of the content `View`; use `svgWith_` to
  obtain all three ambiently from the enclosing component.

### Fixed

- **Static mounts now carry their `StaticKey` as the diff key.** A
  `VCompStatic` node had no `key`, so two different `static` sites at the
  same position compared equal in the differ: the first component stayed
  mounted and the second's `diffProps` ran against it with props of an
  unrelated type. `buildComp` now sets `key` to the mount's `StaticKey`
  (unique per `static` site) when no explicit key is given, so swapping
  `vcomp_ (static (mountStatic A))` for `vcomp_ (static (mountStatic B))`
  replaces the child. Siblings built from one `static` site still diff
  positionally.

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
  secure context (HTTPS or `localhost`); on browsers without the API
  (e.g. Firefox) the error callback fires and `cookieChangeSub` is a no-op.

- **`canvasSub`.** New `Miso.Subscription.Canvas` module. `canvasSub` drives
  a `<canvas>` in a tight `requestAnimationFrame` loop, bypassing virtual DOM
  construction entirely — unlike `Miso.Canvas`, whose `draw` runs during the
  diffing process on discrete events. Pair it with `onCreatedWith` /
  `onDestroyed` and `startSub` / `stopSub` to start the loop when the canvas
  mounts and stop it on unmount. The draw callback receives each frame's
  high-resolution timestamp and a snapshot of the component's current model
  (see the `Sub` change below), and the queued frame is cancelled before the
  callback is freed on teardown.

- **`Miso.Trace`.** A browser-console analogue of `Debug.Trace` for
  debugging pure code such as `view` functions or helpers called from
  `update`. `trace`, `traceId`, `traceWith`, `traceShow`, `traceShowId`,
  `traceShowWith`, `traceM` and `traceShowM` log with `console.log`; the
  `traceWarn*` and `traceError*` families log with `console.warn` and
  `console.error` respectively, gaining the browser's severity filtering
  and stack traces. `traceTo` generalises over any `MisoString -> IO ()`
  console function from `Miso.FFI`. Like `Debug.Trace`, these are built on
  `unsafePerformIO` and are a debugging aid only.

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

- **Every exported name is documented.** `cabal haddock` reported 118
  undocumented exports across 34 modules — mostly the Lynx event payloads,
  decoders, method parameter records and `Events` maps under
  `Miso.Native.Element.*`. All now carry Haddock.

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

- **Types that were reachable but not exported.** Several types appeared in
  exported signatures without being exported themselves, so callers could not
  name them: `Consumed` (the payload of `Miso.Native.Element.List.Method`'s
  callback), `GetTextBoundingRect` (the parameter of `getTextBoundingRect`),
  `ListItemInfo`, `AnimationType` and `UIAppearanceDetailEventType` (field
  types of exported Lynx event records), and `ComponentIds` (the type of
  `ComponentState`'s `_componentChildren`). `Miso.JSON` now exports `ToJSON`
  with both methods — `toJSONList` was hidden, so it could not be overridden
  outside the module — along with the four generic-deriving classes missing
  from its `Generics` group (`GToJSONRep`, `GToJSONSumNullary`,
  `GFromJSONRep`, `GFromJSONSumNullary`). `Miso.Lens.Generic` likewise exports
  the type-level machinery its `HasLens` instances mention (`GSet`,
  `GetFieldType`, `TotalityCheck`, `And`, `Or`).

- **`aeson` cabal flag.** When enabled (`-faeson`, off by default),
  `Miso.JSON` keeps its API but is defined in terms of
  [aeson](https://hackage.haskell.org/package/aeson): `Value`, `Object`, and
  `Parser` become aeson's types, so existing aeson `ToJSON` / `FromJSON`
  instances work directly with `Miso.Fetch`, `Miso.WebSocket`, and the event
  decoders. Signatures are unchanged — the accessors still take `MisoString`
  keys, `withArray` still passes the continuation a `[Value]`, `withNumber`
  still passes a `Double`, and `Result` still carries `MisoString` error
  messages. On the JS / WASM backends orphan instances make `JSString` a
  first-class JSON citizen. Miso's own generic-deriving machinery (`GToJSON`
  et al.) is not exported in this mode; aeson's `genericToJSON` / `Options` /
  `camelTo2` are re-exported instead. CI runs the WASM integration suite in
  both modes.

- **`text` cabal flag on WASM.** When enabled (`-ftext`, off by default),
  `MisoString` is `Data.Text.Text` instead of `JSString` on the WASM
  backend too (previously this was only possible on the `VANILLA` / SSR
  build). `Data.JSString` remains the FFI boundary type, so DOM writes
  still convert `Text -> JSString` on the way out. Number formatting and
  parsing take advantage of this to avoid unnecessary FFI round trips:
  `toMisoString` on `Int` / `Word` / `Double` / `Float` builds `Text`
  directly via `Data.Text.Lazy.Builder` (`decimal` / `realFloat`) instead
  of allocating a throwaway `JSVal` via JS's `.toString()`, since GHC's
  `Show` formatting is what these functions target on this backend
  regardless. Likewise,
  `fromMisoString` on `Int` / `Word` / `Double` / `Float` parses directly
  with `Data.Text.Read` instead of round-tripping through
  `JSString`/`parseInt`/`parseFloat`, while reproducing the JS parsers'
  semantics: leading/trailing whitespace and trailing garbage are
  ignored, a leading `+`/`-` is accepted, and integers with a `0x`/`0X`
  prefix parse as hexadecimal. CI gained a `playwright-wasm-aeson-text`
  target that runs the WASM integration suite with both the `aeson` and
  `text` flags enabled together.

### Changed

- **Breaking: `View` and `Attribute` gained type parameters.**
  `View context model action` and `Attribute model action`. This lets event
  handlers read the current `model` and supports the native dual-thread
  `static` handler protocol. `VNode` now carries a `DirectEvents` set, the
  key moved into `SomeComponent (Maybe Key) …`, and `VComp` / `VCompStatic` /
  `SomeStaticComponent` were restructured. Downstream `view` and attribute
  signatures must be updated accordingly.

- **Breaking: `Sub` gained a `model` type parameter.** `Sub action` is now
  `Sub model action`, and a subscription receives a second argument — an
  `IO model` that returns a snapshot of the component's current model:
  `type Sub model action = Sink action -> IO model -> IO ()`. This lets
  long-running subscriptions (like `canvasSub`) read the latest model
  without threading it through actions. All bundled subscriptions were
  updated; user-defined subscriptions that ignore the model need only accept
  (and discard) the extra argument, e.g.
  `tickSub sink _ = forever (threadDelay delay >> sink Tick)`. `mapSub`,
  `createSub`, and `startSub` were updated accordingly.

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

### Removed

- **`Miso.String.QQ`.** The `misoString` QuasiQuoter for multiline
  `MisoString` literals is gone. GHC's `MultilineStrings` extension
  (GHC 9.12+) covers the use case directly — enable the pragma and write
  triple-quoted `MisoString` literals. (`Miso.FFI.QQ` and `Miso.Lens.TH`,
  the other `template-haskell`-flag modules, are unaffected.)

### Fixed

- **`Miso.Fetch`'s `none` response type no longer double-fires the success
  callback.** `fetchCore` called the success callback directly for
  `responseType == "none"` and then fell through into a second, unconditional
  `.then` that called it again with `body: undefined`. Every `post*`/`put*`
  variant that discards the response body (`postJSON`, `postJSON_`, `putText`,
  `putBlob_`, etc.) dispatched its success action twice per request.

- **Native: attribute removal actually removes the attribute.** The MTS
  drawing context's `removeAttribute` called `__SetAttribute(node, key, '')`.
  The engine's `Element::SetAttribute` (`lynx/core/renderer/dom/element.cc`)
  only takes the removal branch when the value is lepus-empty
  (`null`/`undefined`) — an empty string is an ordinary string value, so it
  was stored in `updated_attr_map_` instead of being removed. Every prop
  diffed off a native element (`dom.ts`'s `diffProps`, which routes native
  removals through this path) was setting it to `''` rather than clearing
  it. Now passes `null`.

- **`rAFSub` now cancels the pending animation frame on unsubscribe.**
  Release freed the `requestAnimationFrame` callback without cancelling the
  frame already queued in the browser; the next frame then invoked a freed
  callback and crashed the WASM RTS with `internal error: stg_ap_p_ret`.
  `Miso.Canvas`'s `draw` also moved from a `syncCallback` to an
  `asyncCallback`, fixing a `schedule: re-entered unsafely` crash when a
  component unmounted mid-diff.

- **Non-bubbling media events are registered in the capture phase.**
  `durationchange`, `loadeddata`, `loadedmetadata` and `loadstart` do not
  bubble, so their delegated listeners — registered in the bubble phase —
  never received them and `onLoadedMetadata` and friends silently never
  fired. They are now registered with capture, like the other non-bubbling
  entries in `mediaEvents`.

- **Native: the layout custom event is recognised under its released name.**
  Released Lynx engines (e.g. LynxExplorer apps) emit it as `layout`, while
  newer Lynx sources emit `layoutchange`; miso only listened for the latter,
  so `onLayoutChange` never fired on released engines. `onLayout` /
  `onLayoutMainWith` are added as aliases so apps can bind both when the
  host engine version is unknown.

- **Native: `consumeSlideEvent_` sends the shape Lynx expects.** Lynx parses
  `consume-slide-event` as `[start, end]` angle-range pairs (degrees,
  -180..180), but the binding serialised a flat list of angles instead of
  paired ranges, a shape the engine silently ignores.

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

- **Non-bubbling `mouseleave`/`pointerleave` are registered in the capture
  phase.** Neither event bubbles per the DOM spec (unlike `mouseout` /
  `pointerout`, which correctly bubble), but the delegated listener was
  registered in the bubble phase, so `onMouseLeave` / `onPointerLeave`
  handlers on any non-root element silently never fired. Same bug class as
  the non-bubbling media events fix above, extended to these two.

- **`vcomp` was misused as a synonym for `component` in `Miso.hs`'s
  documentation.** `vcomp` builds a `VCompStatic` from a `StaticPtr` (the
  static-component feature), not a `Component` from `model` / `update` /
  `view` functions. The module's own "Your first Component" example and two
  other doc snippets used `vcomp` where `component` was meant, so copying
  them verbatim would not typecheck.

- **`MisoString`'s `drop` is code-point-based on WASM, matching `take` /
  `length`.** `take` / `length` were made code-point-based to fix
  astral-character (e.g. emoji) miscounting, but `drop` was left on raw
  UTF-16 slicing. Since `splitAt` is defined as `(take n xs, drop n xs)`,
  the two disagreed on where position `n` falls for any string containing
  an astral character before it, corrupting the split.

- **`-ftext` `parseInt` mis-parses negative hex.** `"-0x1A"` checked for a
  `0x` / `0X` prefix before stripping a sign, so it never matched and fell
  through to a decimal parse of `"0x1A"`, silently returning `0` instead of
  `-26`. The sign is now stripped first, then the remainder is checked for
  a hex prefix.

- **`eventJSON` decodes a null/undefined path as `null` instead of
  crashing.** A decoder path landing on `null`/`undefined` — `relatedTarget`,
  `currentTarget`, `form`, `list`, etc. are all legitimately null/undefined
  on many real DOM events — hit `'length' in obj` on the nullish value and
  threw `TypeError`, crashing event dispatch instead of decoding the field
  as `null`. An intermediate nullish step one segment earlier had the same
  problem; both are now handled.
  
- **`freeLifecycleHooks` frees a component's `mount`/`unmount` callbacks
  again.** It read the `mount`/`unmount` fields off the component's own
  rendered content root instead of the `VComp` wrapper node that actually
  holds them (reachable one hop up, via the content root's `parent` link),
  so `fromJSVal` always failed and `freeFunction` was never called. Every
  non-root `Component` unmount — normal teardown and every GHCi hot-reload
  cycle — leaked the closures `mountCallback`/`unmountCallback` capture,
  which includes the whole `initialize` closure (`app`, `events`, `sink`,
  `model`). See Note [Freeing event handler callbacks] in `Miso.Runtime`.

### Performance

- **Short-lived `JSVal` handles are freed eagerly in the WASM runtime.**
  On the WASM backend every `JSVal` carries a weak pointer with a C
  finalizer, and the RTS copies all of them at every GC — so the hundreds of
  scratch handles `buildVTree` allocates per frame made GC pauses scale with
  handle churn (~100 ms pauses with ~50 KB of live data in profiling). The
  runtime now releases handles nothing else can reach via the new
  `Miso.DSL.freeJSVal` (`GHC.Wasm.Prim.freeJSVal` on WASM, a no-op on other
  backends), and event handler callbacks are freed when their vtree is
  replaced. Measured on miso-mario, `C_FINALIZER_LIST` copied per GC dropped
  from 7.3 MB to 2.3 MB. See Note [Freeing VTree handles] in `Miso.Runtime`.

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
