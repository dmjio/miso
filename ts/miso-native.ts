import {
  TextEncoder,
} from "text-encoding";

import JSBI from "jsbi";

/* Polyfills for native, these come first */
globalThis['TextEncoder'] = TextEncoder as any;
globalThis['BigInt'] = JSBI.BigInt as any;
globalThis['JSBI'] = JSBI;

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
