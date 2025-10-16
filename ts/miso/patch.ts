import { NodeId, ComponentId, Context } from './types';

/* The environment contains a mapping of all components and a read-only JSON rep. of its model
   N.B. no virtual DOM is present here.

   'T' is abstract over any render tree Node type.
*/
export type Environment<T> = Record <ComponentId, ComponentInfo<T>>;

/* Object representation of Model */
export type Model = Object;

/* Information about the current component that lives on the render thread */
export type ComponentInfo<T> = {
  model: Object; /* read-only access to the model */
  nodeMap: NodeMap<T>;
};

/* Convenience table to allow O(1) application of DOM references */
export type NodeMap<T> = Record <number, T>;

/* Function for patch application */
export function applyPatch<T> (context: Context<T>, message: MESSAGE, environment: Environment<T>) {
    let map: NodeMap<T> = null;
    switch (message.type) {
        case "mount":
            break;
        case "unmount":
            break;
        case "events":
            /* apply events */
            break;
        case "insertBefore":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
              context.insertBefore
                (map[message.parent.nodeId], map[message.child.nodeId], map[message.node.nodeId]);
            }
            break;
        case "swapDOMRefs":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
              context.swapDOMRefs (map[message.nodeA.nodeId], map[message.nodeB.nodeId], map[message.parent.nodeId]);
            }
            break;
        case "createElement":
            break;
        case "createElementNS":
            break;
        case "createTextNode":
            break;
        case "setAttribute":
            break;
        case "appendChild":
            break;
        case "replaceChild":
            break;
        case "removeAttribute":
            break;
        case "setTextContent":
            break;
        case "setInlineStyle":
            break;
        case "flush":
            break;
    }
}

/* Message protocol for bidirectional synchronization between MTS / BTS */
export type MESSAGE = COMPONENT | EVENTS | PATCH;
export type COMPONENT = MOUNT | UNMOUNT;
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

export type UNMOUNT = {
  type: "unmount";
  componentId: ComponentId;
};

export type MOUNT = {
  type: "mount";
  componentId: ComponentId;
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
  type: "createElement"
};

export type CreateElementNS = {
  componentId: ComponentId,
  nodeId: number,
  type: "createElementNS"
};

export type CreateTextNode = {
  componentId: ComponentId,
  nodeId: number,
  type: "createTextNode"
};

export type SetAttribute = {
  componentId: ComponentId,
  type: "setAttribute"
};

export type AppendChild = {
  componentId: ComponentId,
  type: "appendChild"
};

export type ReplaceChild = {
  componentId: ComponentId,
  type: "replaceChild"
};

export type RemoveAttribute = {
  componentId: ComponentId,
  type: "removeAttribute"
};

export type SetTextContent = {
  componentId: ComponentId,
  type: "setTextContent"
};

export type SetInlineStyle = {
  componentId: ComponentId,
  type: "setInlineStyle"
};

export type Flush = {
  componentId: ComponentId,
  type: "flush"
};
