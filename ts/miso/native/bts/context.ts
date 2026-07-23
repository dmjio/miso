import {
  NodeId,
  getDOMRef,
  VComp,
  DrawingContext,
  EventContext,
  EventCapture,
  delegateEvent,
  VTree,
  AddClass,
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
} from '../../../miso';

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
  nextSibling : (x : VComp<NodeId>) => {
    return getDOMRef(x.nextSibling);
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
  insertBefore : (parent: NodeId, node: NodeId, child: NodeId) => {
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
     /* send patches from BTS to MTS for application */
     if (globalThis['initialDraw']) {
       globalThis['initialDraw'] = false;
     } else {
       const context = lynx.getCoreContext();
       if (context)
         context.dispatchEvent({ type: 'Miso.patches', data: globalThis['patches'] as Array<PATCH> });
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
