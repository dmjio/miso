import {
  diff,
  hydrate,
  version,
  callBlur,
  callFocus,
  callSelect,
  callSetSelectionRange,
  eventJSON,
  fetchCore,
  integrityCheck,
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
} from './miso';

import {
  drawingContext,
  eventContext,
  hydrationContext,
  componentContext,
} from './miso/context/dom';

/* export globally */
globalThis['miso'] = {
    hydrationContext,
    eventContext,
    drawingContext,
    componentContext,
    diff,
    hydrate,
    version,
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
    integrityCheck,
    delegateEvent,
    delegator : eventContext.delegator,
    setDrawingContext : function (name) {
      // dmj: this looks for a custom globally defined rendering / event context
      // to be used when targetting custom renderers (e.g. lynxjs).
      const drawing = globalThis[name]['drawingContext'];
      const events = globalThis[name]['eventContext'];
      const components = globalThis[name]['componentContext'];
      if (!drawing) {
        console.error('Custom rendering engine ("drawingContext") is not defined at globalThis[name].drawingContext', name);
      }
      if (!events) {
        console.error('Custom event delegation ("eventContext") is not defined at globalThis[name].eventContext', name);
      }
      if (!components) {
        console.error('Custom component context ("componentContext") is not defined at globalThis[name].componentContext', name);
      }
      globalThis['miso']['drawingContext'] = drawing;
      globalThis['miso']['eventContext'] = events;
      globalThis['miso']['componentContext'] = components;
    }
};
