import {
  diff,
  hydrate,
  version,
  delegate,
  callBlur,
  callFocus,
  eventJSON,
  fetchJSON,
  undelegate,
  shouldSync,
  integrityCheck,
  setComponent,
} from './miso';

import { context } from './miso/context/dom';

/* export globally */
globalThis['miso'] = {};
globalThis['miso']['diff'] = diff;
globalThis['miso']['hydrate'] = hydrate;
globalThis['miso']['version'] = version;
globalThis['miso']['delegate'] = delegate;
globalThis['miso']['callBlur'] = callBlur;
globalThis['miso']['callFocus'] = callFocus;
globalThis['miso']['eventJSON'] = eventJSON;
globalThis['miso']['fetchJSON'] = fetchJSON;
globalThis['miso']['undelegate'] = undelegate;
globalThis['miso']['shouldSync'] = shouldSync;
globalThis['miso']['integrityCheck'] = integrityCheck;
globalThis['miso']['setComponent'] = setComponent;
globalThis['miso']['context'] = context;
globalThis['miso']['setDrawingContext'] = function (name) {
    const ctx = globalThis[name];
    if (!ctx) {
      console.warn('Custom rendering engine is not defined', name, globalThis[name]);
    } else {
      globalThis['miso']['context'] = ctx;
    }
}
