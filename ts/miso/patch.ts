import { NodeId, ComponentId, EventCapture, Context } from './types';
import { delegate, undelegate } from './event';

/* The components contains a mapping of all components and a read-only JSON rep. of its model
   N.B. no virtual DOM is present here.

   'T' is abstract over any render tree Node type.
*/
export type Components<T> = Record <ComponentId, Component<T>>;

/* Information about the current component that lives on the render thread */
export type Component<T> = {
  model: Object; /* read-only access to the model, model must be serializable */
  nodes: Record<number, T>;
  mountPoint: T; /* acts as parent */
};

/* Convenience table to allow O(1) application of DOM references */
export type NodeMap<T> = Record <number, T>;

/* Function for patch application */
export function patch<T> (context: Context<T>, patch: PATCH, components: Components<T>) {
    let map: NodeMap<T> = null;
    let newNode = null;
    switch (patch.type) {
        case "insertBefore":
            map = components[patch.componentId]?.nodes;
            if (map) {
              newNode = context.insertBefore(map[patch.parent.nodeId], map[patch.child.nodeId], map[patch.node.nodeId]);
              newNode['nodeId'] = patch.node.nodeId;
              map['nodeId'] = newNode;
            }
            break;
        case "swapDOMRefs":
            map = components[patch.componentId]?.nodes;
            if (map) {
              context.swapDOMRefs (map[patch.nodeA.nodeId], map[patch.nodeB.nodeId], map[patch.parent.nodeId]);
            }
            break;
        case "createElement":
            map = components[patch.componentId]?.nodes;
            if (map) {
                newNode = context.createElement (patch.tag);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "createElementNS":
            map = components[patch.componentId]?.nodes;
            if (map) {
                newNode = context.createElementNS (patch.namespace, patch.tag);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "createTextNode":
            map = components[patch.componentId]?.nodes;
            if (map) {
                newNode = context.createTextNode (patch.text);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "setAttribute":
            map = components[patch.componentId]?.nodes;
            if (map) {
              context.setAttribute (map[patch.nodeId], patch.key, patch.value);
            }
            break;
        case "appendChild":
            map = components[patch.componentId]?.nodes;
            if (map) {
              context.appendChild (map[patch.parent], map[patch.child]);
            }
            break;
        case "replaceChild":
            map = components[patch.componentId]?.nodes;
            if (map) {
              context.replaceChild (map[patch.parent], map[patch.new], map[patch.current]);
            }
            break;
        case "removeAttribute":
            map = components[patch.componentId]?.nodes;
            if (map) {
              context.removeAttribute (map[patch.nodeId], patch.key);
            }
            break;
        case "setTextContent":
            map = components[patch.componentId]?.nodes;
            if (map) {
              context.setTextContent (map[patch.nodeId], patch.text);
            }
            break;
        case "setInlineStyle":
            map = components[patch.componentId]?.nodes;
            if (map) {
                context.setInlineStyle (patch.current, patch.new, map[patch.nodeId]);
            }
            break;
        case "flush":
            context.flush ();
            break;
    }
}

/* addEventListener : (mount : T, event : string, listener : any, capture : boolean) => void; */
export function registerEvents<T> (context: Context<T>, e: EVENTS, components: Components<T>) {
  var component = components[e.componentId];
  if (component) {
      /* listener needs to be from context<T> */
      var listener = undefined;
      let debug = false;
      delegate (component.mountPoint, e.events, listener, debug, context);
  }
}

export function unregisterEvents<T> (context: Context<T>, e: EVENTS, components: Components<T>) {
  var component = components[e.componentId];
  if (component) {
      /* listener needs to be from context<T> */
      var listener = undefined;
      let debug = false;
      undelegate (component.mountPoint, e.events, listener, debug, context);
  }
}

/* Message protocol for bidirectional synchronization between MTS / BTS */
export type MESSAGE = EVENTS | PATCHES | MODEL_HYDRATION;

export type PATCH
  = InsertBefore
  | SwapDOMRefs
  | CreateElement
  | CreateElementNS
  | CreateTextNode
  | SetAttribute
  | AppendChild
  | ReplaceChild
  | RemoveAttribute
  | SetTextContent
  | SetInlineStyle
  | Flush;

export type MODEL_HYDRATION = {
  componentId: ComponentId,
  type: "model_hydration";
  events: Array<EventCapture>;
};

export type EVENTS = {
  componentId: ComponentId,
  type: "events";
  events: Array<EventCapture>;
};

export type PATCHES = {
  type: "patches";
  patches: Array<PATCH>;
};

export type InsertBefore = {
  componentId: ComponentId,
  type: "insertBefore",
  parent: NodeId,
  child: NodeId,
  node: NodeId
};

export type SwapDOMRefs = {
  componentId: ComponentId,
  nodeA: NodeId,
  nodeB: NodeId,
  parent: NodeId,
  type: "swapDOMRefs"
};

export type CreateElement = {
  componentId: ComponentId,
  nodeId: number,
  tag: string,
  type: "createElement"
};

export type CreateElementNS = {
  componentId: ComponentId,
  nodeId: number,
  tag: string,
  namespace: string,
  type: "createElementNS"
};

export type CreateTextNode = {
  componentId: ComponentId,
  nodeId: number,
  text: string,
  type: "createTextNode"
};

export type SetAttribute = {
  componentId: ComponentId,
  key: string,
  value: string,
  nodeId: number,
  type: "setAttribute"
};

export type AppendChild = {
  componentId: ComponentId,
  parent: number,
  child: number,
  type: "appendChild"
};

export type ReplaceChild = {
  componentId: ComponentId,
  current: number,
  new: number,
  parent: number,
  type: "replaceChild"
};

export type RemoveAttribute = {
  componentId: ComponentId,
  type: "removeAttribute",
  nodeId: number,
  key: string,
};

export type SetTextContent = {
  componentId: ComponentId,
  type: "setTextContent",
  nodeId: number,
  text: string,
};

export type SetInlineStyle = {
  componentId: ComponentId,
  new : Record<string, any>,
  current : Record<string, any>,
  nodeId: number,
  type: "setInlineStyle"
};

export type Flush = {
  componentId: ComponentId,
  type: "flush"
};
