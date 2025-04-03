import { diff } from './miso/dom';
import { delegate, undelegate, eventJSON } from './miso/event';
import { hydrate, integrityCheck } from './miso/hydrate';
import { version, callFocus, callBlur, setBodyComponent } from './miso/util';
import { VTree, VNode, VText, VComp, Props, CSS, Events, NS, DOMRef, EventCapture, EventObject, Options } from './miso/types';
import { vcomp, vtree, vtext } from './miso/smart';

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
  undelegate,
  integrityCheck,
  setBodyComponent,
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
  vtree,
  vtext,
  vcomp,
};
