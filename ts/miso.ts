import { diff } from './miso/dom';
import { eventJSON, delegateEvent } from './miso/event';
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
   getRandomValues,
   splitmix32,
   getDOMRef
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
    EventContext,
    DrawingContext,
    HydrationContext,
    NodeId,
    VTreeType,
} from './miso/types';

import { patch, PATCH, Component, Runtime } from './miso/patch';
import { patchDrawingContext } from './miso/context/patch';
import { vcomp, vnode, vtext } from './miso/smart';

/* Top-level re-export */
export {

  /* Context */
  EventContext,
  DrawingContext,
  HydrationContext,
  patchDrawingContext,

  /* Functions */
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
  getRandomValues,
  splitmix32,
  delegateEvent,
  getDOMRef,

  /* Types */
  VTree,
  VTreeType,
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
  NodeId,
  PATCH,
  Runtime,
  Component,

  /* Smart constructors */
  vnode,
  vtext,
  vcomp
};
