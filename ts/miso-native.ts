import {
  TextEncoder,
  TextDecoder,
} from "text-encoding";

import JSBI from "jsbi";

/* Polyfills for native, these come first */
globalThis['TextEncoder'] = TextEncoder as any;
globalThis['TextDecoder'] = TextDecoder as any;
globalThis['BigInt'] = JSBI.BigInt as any;
globalThis['JSBI'] = JSBI;

/* Native-only entry. Lynx's *native* background thread exposes a web-compatible
   Fetch API as `lynx.fetch`, not as a bare global, so Miso.Fetch (fetchCore,
   which calls the global `fetch`) needs it aliased. Resolved lazily so it
   survives load ordering. Guarded by `typeof fetch` so that under lynx-web —
   where the BTS runs in a web worker that already has a real `fetch` — we leave
   it alone. (The plain browser/WASM build uses ts/index.ts and never loads this
   file.) */
if (typeof (globalThis as any)['fetch'] === 'undefined') {
  (globalThis as any)['fetch'] = (input: any, init?: any) =>
    (globalThis as any)['lynx'].fetch(input, init);
}

/* Bridge JS console diagnostics to the native (iOS/Android) device log, gated
   behind `globalThis.debug`.
   Lynx routes background-thread `console.*` through `__OnBTSConsoleEvent`, whose
   text the host does NOT print to the device syslog — so miso's
   `FFI.consoleError`/`consoleLog` diagnostics are invisible to `idevicesyslog`
   without a LynxDevTool connection. Lynx's error reporter, by contrast, DOES
   surface in the native log (that path is how a `Runtime.hs` exception appears
   as `LynxTemplateRender onErrorOccurred`). Mirror `console.error` through
   `lynx.reportError` (non-fatal) so on-device diagnostics are visible. Each line
   is prefixed `[miso]` so it's greppable.
   The override is always installed but only forwards when `globalThis.debug` is
   truthy — checked per-call, NOT at install time — so enabling it after startup
   works. Turn it on from Haskell via `Miso.Native.FFI.enableDebugging`. */
try {
  if (typeof (lynx as any)['reportError'] === 'function') {
    const report = (msg: string) => {
      try { (lynx as any).reportError(new Error(msg)); } catch (_e) { /* never let logging throw */ }
    };
    const origError = console.error.bind(console);
    console.error = (...args: any[]) => {
      origError(...args);
      if ((globalThis as any)['debug'])
        report('[miso] ' + args.map(a => { try { return String(a); } catch (_e) { return '<?>'; } }).join(' '));
    };
  }
} catch (_e) { /* host may freeze `console`; bridging is best-effort */ }

import {
  diff,
  hydrate,
  version,
  onBTS,
  onMTS,
  callBlur,
  callFocus,
  callSelect,
  callSetSelectionRange,
  eventJSON,
  fetchCore,
  eventSourceConnect,
  eventSourceClose,
  websocketConnect,
  websocketClose,
  websocketSend,
  populateClass,
  updateRef,
  inline,
  typeOf,
  mathRandom,
  getRandomValues,
  splitmix32,
  delegateEvent,
  cookieGet,
  cookieGetAll,
  cookieSet,
  cookieDelete,
  cookieDeleteWith,
} from './miso';

import { bts } from './miso/native/bts';
import { mts } from './miso/native/mts';
import { drawingContext as btsDC, eventContext as btsEC } from './miso/native/bts/context';
import { drawingContext as mtsDC, eventContext as mtsEC } from './miso/native/mts/context';

globalThis['nodeId'] = 1;
globalThis['initialDraw'] = true;

const drawingContext = __BACKGROUND__ ? btsDC : mtsDC;
const eventContext  = __BACKGROUND__ ? btsEC  : mtsEC;

/* Named rendering engine looked up by `renderApp events "native"` (see
   Miso.Native.native) via `setDrawingContext`, and holds `currentPageId`
   set on MTS in mts(). */
globalThis['native'] = {
  drawingContext,
  eventContext,
  currentPageId: undefined,
};

globalThis['miso'] = {
  drawingContext,
  eventContext,
  diff,
  hydrate,
  version,
  onBTS,
  onMTS,
  callBlur,
  callFocus,
  callSelect,
  callSetSelectionRange,
  eventJSON,
  fetchCore,
  eventSourceConnect,
  eventSourceClose,
  websocketConnect,
  websocketClose,
  websocketSend,
  updateRef,
  inline,
  typeOf,
  mathRandom,
  getRandomValues,
  splitmix32,
  populateClass,
  delegateEvent,
  cookieGet,
  cookieGetAll,
  cookieSet,
  cookieDelete,
  cookieDeleteWith,
  delegator: eventContext.delegator,
  setDrawingContext: function (name) {
    const drawing = globalThis[name]['drawingContext'];
    const events  = globalThis[name]['eventContext'];
    if (!drawing)
       console.error('"drawingContext" not defined at globalThis[' + name + '].drawingContext');
    if (!events)
       console.error('"eventContext" not defined at globalThis[' + name + '].eventContext');
    globalThis['miso']['drawingContext'] = drawing;
    globalThis['miso']['eventContext'] = events;
  }
};

/* Shared Lynx interop: bound on globalThis for both threads (MTS + BTS).
   `invokeExec` calls a UI method on a selected element via SelectorQuery and
   returns the result (or error) through callbacks. The element `Method`
   bindings (Miso.Native.Element.*.Method) use it regardless of which thread
   the triggering `update` runs on; callers pick the thread via Haskell's
   `runOnBG` / `runOnMain`. */
globalThis['invokeExec'] = function
  ( selector: string,
    method: string,
    params: Object,
    success: (result: any) => void,
    fail: (result: string) => void
  ) {
   const args = { params, method, success, fail };
   return lynx.createSelectorQuery()
       .select(selector)
       .invoke(args as any)
       .exec();
};

if (__BACKGROUND__) {
  globalThis['lynx'] = lynx;
  globalThis['patches'] = [];
  bts();
} else {
  globalThis['renderPage'] = () => mts();
  globalThis['runWorklet'] = (worklet, params) => worklet(params);
}

/* Polyfills global rAF w/ lynx */
globalThis['requestAnimationFrame'] = lynx['requestAnimationFrame'];
globalThis['cancelAnimationFrame'] = lynx['cancelAnimationFrame'];
globalThis['processData'] = () => {};
