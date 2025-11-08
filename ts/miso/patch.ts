import { ComponentId, EventCapture, DrawingContext, EventContext } from './types';
import { delegate, undelegate } from './event';

/* The components contains a mapping from componentId to component and a read-only JSON rep. of its model

   N.B. no virtual DOM is present here.

   'T' is abstract over any render tree Node type.
*/
export type Components<T> = Record <ComponentId, Component<T>>;

/* Information about the current component that lives on the render thread */
export type Component<T> = {
  model: Object; /* read-only access to the model, model must be serializable */
  nodes: Record<number, T>;
};

/* Convenience table to allow O(1) application of DOM references */
export type NodeMap<T> = Record <number, T>;

/* Function for patch application */
export function patch<T> (context: DrawingContext<T>, patch: PATCH, components: Components<T>) {
    withComponent (components, patch.componentId, (component) => {
        switch (patch.type) {
            case "createElement":
                component.nodes[patch.nodeId] = context.createElement (patch.tag);
                component.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
                break;
            case "createElementNS":
                component.nodes[patch.nodeId] = context.createElementNS (patch.namespace, patch.tag);
                component.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
                break;
                break;
            case "createTextNode":
                component.nodes[patch.nodeId] = context.createTextNode (patch.text);
                component.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
                break;
            case "setAttribute":
                context.setAttribute (component.nodes[patch.nodeId], patch.key, patch.value);
                break;
            case "setAttributeNS":
                context.setAttributeNS (component.nodes[patch.nodeId], patch.namespace, patch.key, patch.value);
                break;
            case "removeChild":
                context.removeChild (component.nodes[patch.parent], component.nodes[patch.child]);
                delete component.nodes[patch.child];
                break;
            case "appendChild":
                context.appendChild (component.nodes[patch.parent], component.nodes[patch.child]);
                break;
            case "setInlineStyle":
                context.setInlineStyle (patch.current, patch.new, component.nodes[patch.nodeId]);
                break;
            case "removeAttribute":
                context.removeAttribute (component.nodes[patch.nodeId], patch.key);
                break;
            case "setTextContent":
                context.setTextContent (component.nodes[patch.nodeId], patch.text);
                break;
            case "flush":
                context.flush ();
                break;
            case "insertBefore":
                /* dmj: swap it in the component environemnt too */
                context.insertBefore(component.nodes[patch.parent], component.nodes[patch.child], component.nodes[patch.node]);
                break;
            case "swapDOMRefs":
                /* dmj: swap it in the component environemnt too */
                context.swapDOMRefs (component.nodes[patch.nodeA], component.nodes[patch.nodeB], component.nodes[patch.parent]);
                break;
            case "replaceChild":
                /* dmj: swap it in the component environemnt too */
                context.replaceChild (component.nodes[patch.parent], component.nodes[patch.new], component.nodes[patch.current]);
                delete component.nodes[patch.current];
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
  | SetAttributeNS
  | AppendChild
  | RemoveChild
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
  parent: number,
  child: number,
  node: number
};

export type SwapDOMRefs = {
  componentId: ComponentId,
  nodeA: number,
  nodeB: number,
  parent: number,
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
  value: any,
  nodeId: number,
  type: "setAttribute"
};

export type SetAttributeNS = {
  componentId: ComponentId,
  key: string,
  value: any,
  nodeId: number,
  type: "setAttributeNS",
  namespace: string,
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

export type RemoveChild = {
  componentId: ComponentId,
  parent: number,
  child: number,
  type: "removeChild"
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
