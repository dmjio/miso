import {
  diff,
  hydrate,
  version,
  delegate,
  callBlur,
  callFocus,
  eventJSON,
  fetchCore,
  undelegate,
  shouldSync,
  getParentComponentId,
  integrityCheck,
  eventSourceConnect,
  eventSourceClose,
  websocketConnect,
  websocketClose,
  websocketSend,
} from './miso';

import { drawingContext, eventContext, hydrationContext } from './miso/context/dom';

/* export globally */
globalThis['miso'] = {};
globalThis['miso']['hydrationContext'] = hydrationContext;
globalThis['miso']['eventContext'] = eventContext;
globalThis['miso']['drawingContext'] = drawingContext;
globalThis['miso']['diff'] = diff;
globalThis['miso']['hydrate'] = hydrate;
globalThis['miso']['version'] = version;
globalThis['miso']['delegate'] = delegate;
globalThis['miso']['callBlur'] = callBlur;
globalThis['miso']['callFocus'] = callFocus;
globalThis['miso']['eventJSON'] = eventJSON;
globalThis['miso']['fetchCore'] = fetchCore;
globalThis['miso']['eventSourceConnect'] = eventSourceConnect;
globalThis['miso']['eventSourceClose'] = eventSourceClose;
globalThis['miso']['websocketConnect'] = websocketConnect;
globalThis['miso']['websocketClose'] = websocketClose;
globalThis['miso']['websocketSend'] = websocketSend;
globalThis['miso']['undelegate'] = undelegate;
globalThis['miso']['getParentComponentId'] = getParentComponentId;
globalThis['miso']['shouldSync'] = shouldSync;
globalThis['miso']['integrityCheck'] = integrityCheck;
globalThis['miso']['setDrawingContext'] = function (name) {
    const drawing = globalThis[name]['drawingContext'];
    const events = globalThis[name]['eventContext'];
    if (!drawing) {
      console.warn('Custom rendering engine is not defined', name, globalThis[name]['drawingContext']);
    } else if (!events) {
      console.warn('Custom event delegation is not defined', name, globalThis[name]['eventContext']);
    } else {
      globalThis['miso']['drawingContext'] = drawing;
      globalThis['miso']['eventContext'] = events;
      // dmj: TODO, update miso-lynx to reflect this change
    }
}
