/*

[MTS notes]
  - Setup main thread runtime state to hold components, along with their models, and root nodeId.
  - Setup event listeners for background thread communication and main thread runtime state.
    - Add handler for initial event delegation (received from bg thread).
      - On events, build stack for background thread processing, post via WebWorker to bg thread.
    - Add handler for patch application and component creation.
      - Place nodeId on every DOM node for event delegation (easier to build event stack).
    - Add handler for receiving updated model from bg thread
      - This is meant to be used by background thread for read-only purposes
    - Main thread events require modification to `miso` itself.
      - We'd need to add top-level handlers that take `mainThread :: Object -> IO ()`
        - where `Object` is a JSON'ified version of `model`.

*/

import
  { PATCH,
    Runtime,
  } from "../../miso";

import type { ElementRef } from "@lynx-js/type-element-api";

import
  { drawingContext
  , destroyNodeEvents
  , destroyListState
  } from './mts/context';

export function mts () {
  const page = __CreatePage("0", 0);
  const pageId = __GetElementUniqueID(page);
  __SetCSSId([page], 0);
  globalThis['native']['currentPageId'] = pageId;
  globalThis['page'] = page;

  /* sets page as root node in document */
  globalThis['document'] = {} as any;
  globalThis['document']['body'] = page as any;
  initMainThreadProcessing();
}

/* Method to initialize main thread event handling / processing */
function initMainThreadProcessing () {
  const context = lynx.getJSContext();

  /* initialize runtime state */
  const runtime : Runtime<ElementRef> = {
    nodes : {}
  };

  runtime.nodes[0] = globalThis['page'];
  globalThis['runtime'] = runtime;

  context.addEventListener("Miso.patches", (messages : MessageEvent<Array<PATCH>>) => {
    for (const m of messages.data) {
       processMessage(m,runtime);
    }
    if (messages.data.length > 0) {
       drawingContext.flush();
    }
  });
}

/* main thread message processing */
function processMessage (m : PATCH, runtime) {
  let node = null;
  switch (m.type) {
    case "createElement":
      node = drawingContext.createElement (m.tag);
      __SetConfig (node, { nodeId : m.nodeId });
      runtime.nodes[m.nodeId] = node;
      break;
    case "createTextNode":
      node = drawingContext.createTextNode (m.text);
      __SetConfig (node, { nodeId : m.nodeId });
      runtime.nodes[m.nodeId] = node;
      break;
    case "createElementNS":
      node = drawingContext.createElementNS (m.namespace, m.tag);
      __SetConfig (node, { nodeId : m.nodeId });
      runtime.nodes[m.nodeId] = node;
      break;
    case "swapDOMRefs":
      drawingContext.swapDOMRefs
        (runtime.nodes[m.nodeA], runtime.nodes[m.nodeB], runtime.nodes[m.parent]);
      break;
    case "insertBefore":
      drawingContext.insertBefore
        (runtime.nodes[m.parent], runtime.nodes[m.node], runtime.nodes[m.child]);
      break;
    case "setAttribute":
      drawingContext.setAttribute (runtime.nodes[m.nodeId], m.key, m.value);
      break;
    case "setAttributeNS":
      drawingContext.setAttributeNS (runtime.nodes[m.nodeId], m.namespace, m.key, m.value);
      break;
    case "setTextContent":
      drawingContext.setTextContent (runtime.nodes[m.nodeId], m.text);
      break;
    case "appendChild":
      drawingContext.appendChild (runtime.nodes[m.parent], runtime.nodes[m.child]);
      break;
    case "removeChild": {
      const removed = runtime.nodes[m.child];
      drawingContext.removeChild (runtime.nodes[m.parent], removed);
      dropChildren (runtime.nodes, removed);
      break;
    }
    case "replaceChild": {
      const replaced = runtime.nodes[m.current];
      drawingContext.replaceChild (runtime.nodes[m.parent], runtime.nodes[m.new], replaced);
      dropChildren (runtime.nodes, replaced);
      break;
    }
    case "removeAttribute":
      drawingContext.removeAttribute (runtime.nodes[m.nodeId], m.key);
      break;
    case "setInlineStyle":
      drawingContext.setInlineStyle (m.current, m.new, runtime.nodes[m.nodeId]);
      break;
    case "addClass":
      drawingContext.addClass (m.key, runtime.nodes[m.nodeId]);
      break;
    case "removeClass":
      drawingContext.removeClass (m.key, runtime.nodes[m.nodeId]);
      break;
    case "addEvent":
      drawingContext.addEvent (runtime.nodes[m.nodeId], m.name,
        { capture: m.capture, staticKey: m.staticKey, componentId: m.componentId, options: m.options, direct: m.direct });
      break;
    case "removeEvent":
      drawingContext.removeEvent (runtime.nodes[m.nodeId], m.name, m.capture);
      break;
    case "flush":
      drawingContext.flush ();
      break;
    default:
      console.error('Unknown message received', m);
      break;
  }
}

/* Recursively purge a subtree from runtime.nodes. ElementRef is an opaque
   native handle (no `.children`/`.nodeId` JS properties), so read the id from
   Config and walk children via the element PAPI. `node` is captured before
   detachment, so its subtree is still intact here. */
export function dropChildren (nodeMap: Record<number, ElementRef>, node: ElementRef) {
   const nodeId = __GetConfig(node)?.nodeId as number | undefined;
   if (nodeId !== undefined) {
      delete nodeMap[nodeId];
      // Tear down this node's event state (main-thread routing registry + native
      // direct-bind listeners), else each destroyed node leaks its entries.
      destroyNodeEvents(node, nodeId);
   }
   // Keyed by __GetElementUniqueID rather than nodeId, so this runs
   // unconditionally (not gated on nodeId being set) -- a removed <list>,
   // wherever it sits in this subtree, must not keep its virtualization
   // state (and every ElementRef it holds) alive forever.
   destroyListState(node);
   for (let child = __FirstElement(node) as ElementRef; child; child = __NextElement(child) as ElementRef) {
      dropChildren(nodeMap, child);
   }
}

