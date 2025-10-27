import { NodeId, ComponentId, Context } from './types';

/* The environment contains a mapping of all components and a read-only JSON rep. of its model
   N.B. no virtual DOM is present here.

   'T' is abstract over any render tree Node type.
*/
export type Components<T> = Record <ComponentId, Component<T>>;

/* Information about the current component that lives on the render thread */
export type Component<T> = {
  model: Object; /* read-only access to the model, model must be serializable */
  nodes: Record<number, T>;
  mountPoint: T;
};

/* Convenience table to allow O(1) application of DOM references */
export type NodeMap<T> = Record <number, T>;

/* Function for patch application */
export function patch<T> (context: Context<T>, patch: PATCH, environment: Components<T>) {
    let map: NodeMap<T> = null;
    let newNode = null;
    switch (patch.type) {
        case "insertBefore":
            map = environment[patch.componentId]?.nodes;
            if (map) {
              context.insertBefore
                (map[patch.parent.nodeId], map[patch.child.nodeId], map[patch.node.nodeId]);
            }
            break;
        case "swapDOMRefs":
            map = environment[patch.componentId]?.nodes;
            if (map) {
              context.swapDOMRefs (map[patch.nodeA.nodeId], map[patch.nodeB.nodeId], map[patch.parent.nodeId]);
            }
            break;
        case "createElement":
            map = environment[patch.componentId]?.nodes;
            if (map) {
                newNode = context.createElement (patch.tag);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "createElementNS":
            map = environment[patch.componentId]?.nodes;
            if (map) {
                newNode = context.createElementNS (patch.namespace, patch.tag);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "createTextNode":
            map = environment[patch.componentId]?.nodes;
            if (map) {
                newNode = context.createTextNode (patch.text);
                newNode['nodeId'] = patch.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "setAttribute":
            map = environment[patch.componentId]?.nodes;
            if (map) {
              context.setAttribute (map[patch.nodeId], patch.key, patch.value);
            }
            break;
        case "appendChild":
            map = environment[patch.componentId]?.nodes;
            if (map) {
              context.appendChild (map[patch.parent], map[patch.child]);
            }
            break;
        case "replaceChild":
            map = environment[patch.componentId]?.nodes;
            if (map) {
              context.replaceChild (map[patch.parent], map[patch.new], map[patch.current]);
            }
            break;
        case "removeAttribute":
            map = environment[patch.componentId]?.nodes;
            if (map) {
              context.removeAttribute (map[patch.nodeId], patch.key);
            }
            break;
        case "setTextContent":
            map = environment[patch.componentId]?.nodes;
            if (map) {
              context.setTextContent (map[patch.nodeId], patch.text);
            }
            break;
        case "setInlineStyle":
            map = environment[patch.componentId]?.nodes;
            if (map) {
                context.setInlineStyle (patch.current, patch.new, map[patch.nodeId]);
            }
            break;
        case "flush":
            context.flush ();
            break;
    }
}

/* Message protocol for bidirectional synchronization between MTS / BTS */
export type MESSAGE = EVENTS | PATCH;

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

export type EVENTS = {
  type: "events";
  events: Object;
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
