import {
  diff,
  hydrate,
  version,
  delegate,
  callBlur,
  callFocus,
  eventJSON,
  undelegate,
  integrityCheck,
  setBodyComponent,
} from './miso';

/* export globally */
globalThis['miso'] = {};
globalThis['miso']['diff'] = diff;
globalThis['miso']['hydrate'] = hydrate;
globalThis['miso']['version'] = version;
globalThis['miso']['delegate'] = delegate;
globalThis['miso']['callBlur'] = callBlur;
globalThis['miso']['callFocus'] = callFocus;
globalThis['miso']['eventJSON'] = eventJSON;
globalThis['miso']['undelegate'] = undelegate;
globalThis['miso']['integrityCheck'] = integrityCheck;
globalThis['miso']['setBodyComponent'] = setBodyComponent;
