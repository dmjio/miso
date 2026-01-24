import { diff } from './miso/dom';
import { delegate, eventJSON } from './miso/event';
import { hydrate, integrityCheck } from './miso/hydrate';

import {
   version,
   callFocus,
   callBlur,
   callSelect,
   callSetSelectionRange,
   fetchCore,
   websocketConnect,
   websocketClose,
   websocketSend,
   eventSourceConnect,
   eventSourceClose,
   populateClass,
   updateRef,
   inline,
   typeOf,
   mathRandom,
   splitmix32,
} from './miso/util';

import {
    VTree,
    VNode,
    VText,
    VComp,
    Props,
    CSS,
    Events,
    NS,
    DOMRef,
    EventCapture,
    EventObject,
    Options,
} from './miso/types';

import { patch } from './miso/patch';

import { vcomp, vnode, vtext } from './miso/smart';

/* Top-level re-export */
export {

  /* Functions */
  diff,
  hydrate,
  version,
  delegate,
  callBlur,
  callFocus,
  callSelect,
  callSetSelectionRange,
  eventJSON,
  fetchCore,
  integrityCheck,
  websocketConnect,
  websocketClose,
  websocketSend,
  eventSourceConnect,
  eventSourceClose,
  patch,
  populateClass,
  updateRef,
  inline,
  typeOf,
  mathRandom,
  splitmix32,

  /* Types */
  VTree,
  VComp,
  VText,
  VNode,
  EventCapture,
  EventObject,
  Options,
  CSS,
  Props,
  Events,
  NS,
  DOMRef,
  /* Smart constructors */
  vnode,
  vtext,
  vcomp
};
