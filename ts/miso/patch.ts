import { NodeId, ComponentId, EventCapture, DrawingContext, EventContext } from './types';
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
export function patch<T> (context: DrawingContext<T>, patch: PATCH, components: Components<T>) {
    let map: NodeMap<T> = null;
    let newNode = null;
    withComponent (components, patch.componentId, (component) => {
        map = component.nodeMap;
        switch (patch.type) {
            case "insertBefore":
                newNode = context.insertBefore(map[patch.parent.nodeId], map[patch.child.nodeId], map[patch.node.nodeId]);
                newNode['nodeId'] = patch.node.nodeId;
                map['nodeId'] = newNode;
                break;
            case "swapDOMRefs":
                context.swapDOMRefs (map[patch.nodeA.nodeId], map[patch.nodeB.nodeId], map[patch.parent.nodeId]);
                break;
            case "createElement":
                newNode = context.createElement (patch.tag);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
                break;
            case "createElementNS":
                newNode = context.createElementNS (patch.namespace, patch.tag);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
                break;
            case "createTextNode":
                newNode = context.createTextNode (patch.text);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
                break;
            case "setAttribute":
                context.setAttribute (map[patch.nodeId], patch.key, patch.value);
                break;
            case "appendChild":
                context.appendChild (map[patch.parent], map[patch.child]);
                break;
            case "replaceChild":
                context.replaceChild (map[patch.parent], map[patch.new], map[patch.current]);
                break;
            case "removeAttribute":
                context.removeAttribute (map[patch.nodeId], patch.key);
                break;
            case "setTextContent":
                context.setTextContent (map[patch.nodeId], patch.text);
                break;
            case "setInlineStyle":
                context.setInlineStyle (patch.current, patch.new, map[patch.nodeId]);
                break;
            case "flush":
                context.flush ();
                break;
        }
    });
}

/* addEventListener : (mount : T, event : string, listener : any, capture : boolean) => void; */
export function registerEvents<T> (context: EventContext<T>, e: EVENTS, components: Components<T>) {
  withComponent (components, e.componentId, (component) => {
      /* listener needs to be from context<T> */
      var listener = undefined;
      let debug = false;
      delegate (component.mountPoint, e.events, listener, debug, context);
  });
}

export function unregisterEvents<T> (context: EventContext<T>, e: EVENTS, components: Components<T>) {
  withComponent (components, e.componentId, (component) => {
      /* listener needs to be from context<T> */
      var listener = undefined;
      let debug = false;
      undelegate (component.mountPoint, e.events, listener, debug, context);
  });
}

export function hydrateModel<T> (o: HYDRATION, components: Components<T>) {
  withComponent (components, o.componentId, (component) => {
      component.model = o.model;
  });
}

function withComponent (components, componentId, callback) {
    var component = components[componentId];
    if (component) {
      callback (component);
    } else {
        console.error ('Could not find component at ID: ', componentId);
    }
}

/* Message protocol for bidirectional synchronization between MTS / BTS */
export type MESSAGE = EVENTS | PATCHES | HYDRATION;

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

export type HYDRATION = {
  componentId: ComponentId;
  type: "hydration";
  model: Object;
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
