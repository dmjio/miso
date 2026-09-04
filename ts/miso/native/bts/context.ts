import { getDOMRef } from '../../../miso/util';
import { delegateEvent } from '../../../miso/event';
import { VTreeType } from '../../../miso/types';
import type {
  NodeId,
  DrawingContext,
  EventContext,
  EventCapture,
  VTree,
  AddClass,
  AddEvent,
  RemoveEvent,
  PATCH,
  CreateTextNode,
  CreateElement,
  InsertBefore,
  ProcessEvent,
  CreateElementNS,
  SetAttribute,
  SetAttributeNS,
  SwapDOMRefs,
  AppendChild,
  RemoveChild,
  ReplaceChild,
  SetTextContent,
  SetInlineStyle,
  RemoveAttribute,
  RemoveClass,
} from '../../../miso/types';

function nextNodeId () : number {
  'background only'
  return globalThis['nodeId']++;
}

function addPatch (patch : PATCH) : void {
  'background only'
  globalThis['patches'].push(patch);
}

const eventContext : EventContext<NodeId> = {
  delegator : (mount: NodeId, events: Array<EventCapture>, getVTree : (callback: (vtree: VTree<NodeId>) => void) => void, debug: boolean, eventContext) => {
    const context = lynx.getCoreContext();
    if (!context) return;
    context.addEventListener('Miso.events', (m : MessageEvent<ProcessEvent>) => {
      let stack : Array<NodeId> = m.data.stack.map (function (x) { return { nodeId : x }});
      getVTree((vtree: VTree<NodeId>) => {
        /* The delegator's listeners attach before the initial draw has stored
           the vtree, so events raised in that window see a null vtree. Drop
           them: there is nothing to dispatch on yet. */
        if (!vtree) {
          if (debug) {
            console.warn('Event received before vtree was mounted, dropping', m.data.event);
          }
          return;
        }
        return delegateEvent(m.data.event as Event, vtree, stack, debug, eventContext);
      });
    });
  },
  addEventListener : (mount : NodeId, event : string, listener, capture : boolean) => {
      /* dmj: `addEventListener` is not used on BTS ...
              Sub work, but not the exported Mouse / Window / Keyboard, etc. since they rely on `window` + `addEventListener`.
         dmj: All `addEventListener` must be ported to work w/ MTS, and most likely given a protocol message
              Or use of web workers w/ `async` / `await` to define `runOnMainThread` / `runOnBackground`.
       */
      return;
  },
  isEqual : (x, y) => {
    /* dmj: required */
    return x.nodeId === y.nodeId;
  },
  getTarget : (_ : Event) => {
    /* dmj: not required. */
    return { nodeId: 0 };
  },
  parentNode : (_: NodeId) => {
    /* dmj: not required, uses parent (remove this method?) */
    return { nodeId: 0 };
  }
};

/* Diff + gen. patches on BTS */
const drawingContext : DrawingContext<NodeId> = {
  addClass : (key, n) => {
    let patch : AddClass = {
        type : "addClass",
        nodeId : n.nodeId,
        key
    };
    addPatch(patch);
    return;
  },
  removeClass : (key, n) => {
    const patch : RemoveClass = {
        type : "removeClass",
        nodeId : n.nodeId,
        key
    };
    addPatch(patch);
    return;
  },
  addEvent : (n, name, key) => {
    const patch : AddEvent = {
        type : "addEvent",
        nodeId : n.nodeId,
        name,
        capture : key.capture,
        staticKey : key.staticKey,
        componentId : key.componentId,
        options : key.options,
        direct : key.direct,
    };
    addPatch(patch);
    return;
  },
  removeEvent : (n, name, capture) => {
    const patch : RemoveEvent = {
        type : "removeEvent",
        nodeId : n.nodeId,
        name,
        capture,
    };
    addPatch(patch);
    return;
  },
  nextSibling : (x : VTree<NodeId>) => {
    let sibling = x.nextSibling;
    while (sibling) {
      switch (sibling.type) {
        case VTreeType.VComp:
        case VTreeType.VFrag: {
          const ref = getDOMRef(sibling);
          if (ref) return ref;
          sibling = sibling.nextSibling;
          break;
        }
        default:
          return sibling.domRef;
      }
    }
    return null;
  },
  createTextNode : (text: string) => {
    const nodeId: number = nextNodeId ();
    addPatch ({
        type : "createTextNode",
        text,
        nodeId,
    } as CreateTextNode);
    return { nodeId };
  },
  createElementNS : (ns: string, tag: string) => {
    const nodeId: number = nextNodeId ();
    let patch : CreateElementNS = {
        type : "createElementNS",
        namespace: ns,
        nodeId,
        tag
    };
    addPatch(patch);
    return { nodeId };
  },
  createElement : (tag) => {
    const nodeId: number = nextNodeId ();
    let patch : CreateElement = {
        type : "createElement",
        nodeId,
        tag
    };
    addPatch(patch);
    return { nodeId };
  },
  appendChild : (parent: NodeId, child: NodeId) => {
    let patch : AppendChild = {
        type: "appendChild",
        parent : parent.nodeId,
        child : child.nodeId,
    };
    addPatch(patch);
    return;
  },
  replaceChild : (parent: NodeId, n: NodeId, current: NodeId) => {
    let patch : ReplaceChild = {
        type: "replaceChild",
        parent : parent.nodeId,
        new : n.nodeId,
        current : current.nodeId
    };
    addPatch(patch);
    return;
  },
  removeChild : (parent: NodeId, child: NodeId) => {
    let patch : RemoveChild = {
        type: "removeChild",
        parent : parent.nodeId,
        child : child.nodeId,
    };
    addPatch(patch);
    return;
  },
  insertBefore : (parent: NodeId, node: NodeId, child: NodeId | null) => {
    // A null anchor means "insert at the end" (mirrors DOM `insertBefore(node,
    // null)`), but the wire protocol's `insertBefore` patch has no null anchor
    // representation — degrade to `appendChild` here rather than crash on
    // `child.nodeId` below.
    if (child === null) {
      drawingContext.appendChild(parent, node);
      return;
    }
    let patch : InsertBefore = {
        type: "insertBefore",
        parent : parent.nodeId,
        child : child.nodeId,
        node : node.nodeId,
    };
    addPatch(patch);
    return;
  },
  swapDOMRefs : (nodeA: NodeId, nodeB: NodeId, parent: NodeId) => {
    let patch : SwapDOMRefs = {
        type: "swapDOMRefs",
        parent : parent.nodeId,
        nodeA : nodeA.nodeId,
        nodeB : nodeB.nodeId,
    };
    addPatch(patch);
    return;
  },
  setAttribute: (n, key, value) => {
    let patch : SetAttribute = {
        type : "setAttribute",
        nodeId : n.nodeId,
        key,
        value,
    };
    addPatch(patch);
    return;
  },
  removeAttribute : (n, key) => {
    let patch : RemoveAttribute = {
        type : "removeAttribute",
        nodeId : n.nodeId,
        key,
    };
    addPatch(patch);
    return;
  },
  setAttributeNS: (n, namespace, key, value) => {
    let patch : SetAttributeNS = {
        type : "setAttributeNS",
        nodeId : n.nodeId,
        key,
        value,
        namespace,
    };
    addPatch(patch);
    return;
  },
  setTextContent : (n: NodeId, text: string) => {
    const patch : SetTextContent = {
        type : "setTextContent",
        nodeId : n.nodeId,
        text
    };
    addPatch(patch);
    return;
  },
  setInlineStyle : (cCss, nCss, node) => {
    if (areEqual(cCss, nCss)) return;
    let patch : SetInlineStyle = {
        type : "setInlineStyle",
        nodeId : node.nodeId,
        new: nCss,
        current: cCss,
    };
    addPatch(patch);
    return;
  },
  flush : () => {
     /* Send patches from BTS to MTS for application. During the initial frame the
        MTS paints the tree itself, so the BTS suppresses (drops) its create-patches
        and only keeps the VTree it built (nodeIds stay parity-aligned with the MTS).
        The `initialDraw` latch is cleared ONCE by the runtime after the whole root
        mount completes (Miso.Runtime.initComponent) — NOT here — because the initial
        draw performs one flush per mounted component; flipping it per-flush tripped
        on the first nested child and leaked the rest of the frame as duplicate nodes. */
     const patches = globalThis['patches'] as Array<PATCH>;
     if (!globalThis['initialDraw'] && patches.length > 0) {
       const context = lynx.getCoreContext();
       if (context)
         context.dispatchEvent({ type: 'Miso.patches', data: patches });
     }
     globalThis['patches'] = [];
  },
  getHead : function () {
    /* dmj: unsupported */
    return null;
  },
  getRoot : function () {
    /* pageId = 0 */
    return { nodeId : 0 };
  },
};

function areEqual(a: Object, b: Object) : boolean {
  'background only';
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every(key => a[key] === b[key]);
}

export {
  drawingContext, eventContext
}
