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

import {
  drawingContext,
  eventContext,
  hydrationContext,
} from './miso/context/dom';

/* export globally */
globalThis['miso'] = {
    hydrationContext,
    eventContext,
    drawingContext,
    diff,
    hydrate,
    version,
    delegate,
    callBlur,
    callFocus,
    eventJSON,
    fetchCore,
    eventSourceConnect,
    eventSourceClose,
    websocketConnect,
    websocketClose,
    websocketSend,
    undelegate,
    getParentComponentId,
    shouldSync,
    integrityCheck,
    setDrawingContext : function (name) {
      // dmj: this looks for a custom globally defined rendering / event context
      // to be used when targetting custom renderers (e.g. lynxjs).
      const drawing = globalThis[name]['drawingContext'];
      const events = globalThis[name]['eventContext'];
      if (!drawing) {
        console.error('Custom rendering engine ("drawingContext") is not defined at globalThis[name].drawingContext', name);
      }
      if (!events) {
       console.error('Custom event delegation ("eventContext") is not defined at globalThis[name].eventContext', name);
      }
      globalThis['miso']['drawingContext'] = drawing;
      globalThis['miso']['eventContext'] = events;
    }
};
