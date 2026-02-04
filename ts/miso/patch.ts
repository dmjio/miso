import { ComponentId, EventCapture, DrawingContext } from './types';

/* Information about the current component that lives on the render thread */
export type Component = {
  model : Object,
  /* updated model sent from bg to main */
  mainThreadEvents : Record <string, ((o:Object) => void)>,
  /* dmj: these will need to be sent from bg to main on app load*/
  rootId : number,
  /* the root node to diff from */
}

export type Runtime<T> = {
  nodes : Record <number, T>,
  components : Record <ComponentId, Component>
};

/* Convenience table to allow O(1) application of DOM references */
export type NodeMap<T> = Record <number, T>;

/* Function for patch application */
export function patch<T> (context: DrawingContext<T>, patch: PATCH, runtime: Runtime<T>) {
  switch (patch.type) {
    case "mount":
        runtime.components[patch.componentId] = {
          model : patch.model,
          mainThreadEvents : {},
          rootId : patch.mountPoint
        };
        break;
    case "unmount":
        delete runtime.components[patch.componentId];
        break;
    case "modelHydration":
        runtime.components[patch.componentId].model = patch.model;
        break;
    case "createElement":
        runtime.nodes[patch.nodeId] = context.createElement (patch.tag);
        runtime.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
        break;
    case "createElementNS":
        runtime.nodes[patch.nodeId] = context.createElementNS (patch.namespace, patch.tag);
        runtime.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
        break;
    case "createTextNode":
        runtime.nodes[patch.nodeId] = context.createTextNode (patch.text);
        runtime.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
        break;
    case "setAttribute":
        context.setAttribute (runtime.nodes[patch.nodeId], patch.key, patch.value);
        break;
    case "addClass":
        context.addClass (patch.key, runtime.nodes[patch.nodeId]);
        break;
    case "removeClass":
        context.removeClass (patch.key, runtime.nodes[patch.nodeId]);
        break;
    case "setAttributeNS":
        context.setAttributeNS (runtime.nodes[patch.nodeId], patch.namespace, patch.key, patch.value)
        break;
    case "removeChild":
        context.removeChild (runtime.nodes[patch.parent], runtime.nodes[patch.child]);
        delete runtime.nodes[patch.child];
        break;
    case "appendChild":
        context.appendChild (runtime.nodes[patch.parent], runtime.nodes[patch.child]);
        break;
    case "setInlineStyle":
        context.setInlineStyle (patch.current, patch.new, runtime.nodes[patch.nodeId]);
        break;
    case "removeAttribute":
        context.removeAttribute (runtime.nodes[patch.nodeId], patch.key);
        break;
    case "setTextContent":
        context.setTextContent (runtime.nodes[patch.nodeId], patch.text);
        break;
    case "flush":
        context.flush ();
        break;
    case "insertBefore":
        context.insertBefore(runtime.nodes[patch.parent], runtime.nodes[patch.node], runtime.nodes[patch.child]);
        break;
    case "swapDOMRefs":
        /* dmj: swap it in the runtime environemnt too */
        context.swapDOMRefs (runtime.nodes[patch.nodeB], runtime.nodes[patch.nodeA], runtime.nodes[patch.parent]);
        break;
    case "replaceChild":
        context.replaceChild (runtime.nodes[patch.parent], runtime.nodes[patch.new], runtime.nodes[patch.current]);
        delete runtime.nodes[patch.current];
        break;
    default:
        break;
  }
}

/* Message protocol for bidirectional synchronization between MTS / BTS */
export type PATCH
  = InsertBefore
  | SwapDOMRefs
  | CreateElement
  | CreateElementNS
  | CreateTextNode
  | SetAttribute
  | SetAttributeNS
  | AppendChild
  | RemoveChild
  | ReplaceChild
  | RemoveAttribute
  | SetTextContent
  | SetInlineStyle
  | MountComponent
  | UnmountComponent
  | ModelHydration
  | AddClass
  | RemoveClass
  | ProcessEvent
  | AddEventListeners
  | Flush;

export type AddEventListeners = {
  events: Array<EventCapture>,
  type: "addEventListeners"
};

export type ProcessEvent = {
  stack: Array<number>,
  event: Object, /* Event object, will be JSON'ified */
  type: "processEvent"
};

export type ModelHydration = {
  componentId: ComponentId,
  model: Object,
  type: "modelHydration"
};

export type MountComponent = {
  type: "mount",
  componentId: ComponentId,
  model: Object,
  mountPoint: number
};

export type UnmountComponent = {
  type: "unmount",
  componentId: ComponentId,
};

export type InsertBefore = {
  type: "insertBefore",
  parent: number,
  child: number,
  node: number
};

export type SwapDOMRefs = {
  nodeA: number,
  nodeB: number,
  parent: number,
  type: "swapDOMRefs"
};

export type CreateElement = {
  nodeId: number,
  tag: string,
  type: "createElement"
};

export type CreateElementNS = {
  nodeId: number,
  tag: string,
  namespace: string,
  type: "createElementNS"
};

export type CreateTextNode = {
  nodeId: number,
  text: string,
  type: "createTextNode"
};

export type SetAttribute = {
  key: string,
  value: any,
  nodeId: number,
  type: "setAttribute"
};

export type SetAttributeNS = {
  key: string,
  value: any,
  nodeId: number,
  type: "setAttributeNS",
  namespace: string,
};

export type AppendChild = {
  parent: number,
  child: number,
  type: "appendChild"
};

export type ReplaceChild = {
  current: number,
  new: number,
  parent: number,
  type: "replaceChild"
};

export type RemoveChild = {
  parent: number,
  child: number,
  type: "removeChild"
};

export type RemoveAttribute = {
  type: "removeAttribute",
  nodeId: number,
  key: string,
};

export type RemoveClass = {
  type: "removeClass",
  nodeId: number,
  key: string,
};

export type AddClass = {
  type: "addClass",
  nodeId: number,
  key: string,
};

export type SetTextContent = {
  type: "setTextContent",
  nodeId: number,
  text: string,
};

export type SetInlineStyle = {
  new : Record<string, any>,
  current : Record<string, any>,
  nodeId: number,
  type: "setInlineStyle"
};

export type Flush = {
  type: "flush"
};
