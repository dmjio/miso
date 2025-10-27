import { diff } from './miso/dom';
import { delegate, undelegate, eventJSON } from './miso/event';
import { hydrate, integrityCheck } from './miso/hydrate';

import {
   shouldSync,
   version,
   callFocus,
   callBlur,
   fetchCore,
   getParentComponentId,
   websocketConnect,
   websocketClose,
   websocketSend,
   eventSourceConnect,
   eventSourceClose
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

/* top level re-export */
export {
  /* Functions */
  diff,
  hydrate,
  version,
  delegate,
  callBlur,
  callFocus,
  eventJSON,
  fetchCore,
  undelegate,
  integrityCheck,
  shouldSync,
  getParentComponentId,
  websocketConnect,
  websocketClose,
  websocketSend,
  eventSourceConnect,
  eventSourceClose,
  patch,

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
