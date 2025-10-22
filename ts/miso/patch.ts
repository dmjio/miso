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
        case "events":
            /* registers background events events */

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
              // dmj: reinsert into map here at specified ID
            }
            break;
        case "createElement":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
                var newNode = context.createElement (message.tag);
                newNode['nodeId'] = message.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "createElementNS":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
                var newNode = context.createElementNS (message.namespace, message.tag);
                newNode['nodeId'] = message.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "createTextNode":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
                var newNode = context.createTextNode (message.text);
                newNode['nodeId'] = message.nodeId;
                map['nodeId'] = newNode;
            }
            break;
        case "setAttribute":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
              context.setAttribute (map[message.nodeId], message.key, message.value);
            }
            break;
        case "appendChild":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
              context.appendChild (map[message.parent], map[message.child]);
            }
            break;
        case "replaceChild":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
              context.replaceChild (map[message.parent], map[message.new], map[message.current]);
            }
            break;
        case "removeAttribute":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
              context.removeAttribute (map[message.nodeId], message.key);
            }
            break;
        case "setTextContent":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
              context.setTextContent (map[message.nodeId], message.text);
            }
            break;
        case "setInlineStyle":
            map = environment[message.componentId]?.nodeMap;
            if (map) {
                context.setInlineStyle (message.current, message.new, map[message.nodeId]);
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
