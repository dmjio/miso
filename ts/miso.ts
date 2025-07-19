import { diff } from './miso/dom';
import { delegate, undelegate, eventJSON } from './miso/event';
import { hydrate, integrityCheck } from './miso/hydrate';
import { shouldSync, version, callFocus, callBlur, fetchJSON, getParentComponentId, setVTree } from './miso/util';
import { VTree, VNode, VText, VComp, Props, CSS, Events, NS, DOMRef, EventCapture, EventObject, Options } from './miso/types';
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
  fetchJSON,
  undelegate,
  integrityCheck,
  shouldSync,
  getParentComponentId,
  setVTree,
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
  vcomp,
};
