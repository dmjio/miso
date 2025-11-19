import {
  ComponentId,
  EventCapture,
  DrawingContext,
  VNode,
  NodeId,
  CSS,
  ComponentContext
} from '../types';

/*

  This file is used to provide testing for miso-lynx, or other patch-based architectures
  that use the 2-phase patch + diffing approach.

*/

import {
  PATCH,
  CreateTextNode,
  CreateElement,
  InsertBefore,
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
  UnmountComponent,
  ModelHydration,
} from '../patch';

export function addPatch (componentId: number, patch : PATCH) : void {
  globalThis['patches'].push(patch);
}

export function getPatches () : Array<PATCH> {
  return globalThis['patches'];
}

export function nextNodeId () : number {
  return globalThis['nodeId']++;
}

export function getComponentId () : number {
  return globalThis['componentId'];
}

// dmj: Helper for Object equality.
function areEqual(a: Object, b: Object) {
  const keysA = Object.keys(a);
  const keysB = Object.keys(b);
  if (keysA.length !== keysB.length) return false;
  return keysA.every(key => a[key] === b[key]);
}

export const componentContext : ComponentContext = {
    unmountComponent : function (componentId: ComponentId) {
        let patch : UnmountComponent = {
            type: "unmountComponent",
            componentId,
        };
        addPatch(componentId, patch);
        return;
    },
    modelHydration : function (componentId: ComponentId, model: Object) {
        let patch : ModelHydration = {
            type: "modelHydration",
            model,
            componentId
        };
        addPatch(componentId, patch);
        return;
    }
};

export const patchDrawingContext : DrawingContext<NodeId> = {
  mountComponent : (events : Record<string, boolean>, componentId: number, model: Object) => {
    return;
  },
  nextSibling : (node: VNode<NodeId>) => {
    return node.nextSibling.domRef;
  },
  createTextNode : (value : string, componentId: ComponentId) => {
      const nodeId: number = nextNodeId ();
      let patch : CreateTextNode = {
          text : value,
          type : "createTextNode",
          nodeId,
          componentId
      };
      addPatch(componentId, patch);
      return { nodeId };
  },
  createElementNS : (ns: string, tag: string, componentId: ComponentId) => {
    const nodeId: number = nextNodeId ();
    let patch : CreateElementNS = {
        type : "createElementNS",
        namespace: ns,
        nodeId,
        componentId,
        tag
    };
    addPatch(componentId, patch);
    return { nodeId };
  },
  appendChild : (parent: NodeId, child: NodeId, componentId: ComponentId) => {
    let patch : AppendChild = {
        type: "appendChild",
        parent: parent.nodeId,
        child: child.nodeId,
        componentId,
    };
    addPatch(componentId, patch);
    return;
  },
  replaceChild : (parent: NodeId, n: NodeId, old: NodeId, componentId: ComponentId) => {
    let patch : ReplaceChild = {
        type: "replaceChild",
        parent: parent.nodeId,
        new: n.nodeId,
        current: old.nodeId,
        componentId
    };
    addPatch(componentId, patch);
    return;
  },
  removeChild : (parent: NodeId, child: NodeId, componentId: ComponentId) => {
    let patch : RemoveChild = {
        type: "removeChild",
        parent: parent.nodeId,
        child: child.nodeId,
        componentId
    };
    addPatch(componentId, patch);
    return;
  },
  createElement : (tag, componentId: ComponentId) => {
    const nodeId: number = nextNodeId ();
    let patch : CreateElement = {
        type : "createElement",
        nodeId,
        componentId,
        tag
    };
    addPatch(componentId, patch);
    return { nodeId };
  },
  insertBefore : (parent: NodeId, node: NodeId, child: NodeId, componentId: ComponentId) => {
    let patch : InsertBefore = {
        type: "insertBefore",
        parent: parent.nodeId,
        child: child.nodeId,
        node: node.nodeId,
        componentId
    };
    addPatch(componentId, patch);
    return;
  },
  swapDOMRefs : (a: NodeId, b: NodeId, p: NodeId, componentId: ComponentId) => {
    let patch : SwapDOMRefs = {
        type: "swapDOMRefs",
        parent: p.nodeId,
        nodeA: a.nodeId,
        nodeB: b.nodeId,
        componentId
    };
    addPatch(componentId, patch);
    return;
  },
  setInlineStyle: (cCss: CSS, nCss: CSS, node: NodeId, componentId: ComponentId) => {
    if (areEqual(cCss, nCss)) return;
    let patch : SetInlineStyle = {
        type : "setInlineStyle",
        nodeId : node.nodeId,
        new: nCss,
        current: cCss,
        componentId,
    };
    addPatch(componentId, patch);
    return;
  },
  setAttribute: (node: NodeId, key: string, value: any, componentId: ComponentId) => {
    let patch : SetAttribute = {
        type : "setAttribute",
        nodeId : node.nodeId,
        key,
        value,
        componentId,
    };
    addPatch(componentId, patch);
    return;
  },
  setAttributeNS: (node: NodeId, namespace: string, key: string, value: any, componentId: ComponentId) => {
    let patch : SetAttributeNS = {
        type : "setAttributeNS",
        nodeId : node.nodeId,
        key,
        value,
        namespace,
        componentId,
    };
    addPatch(componentId, patch);
    return;
  },
  removeAttribute : (node: NodeId, key: string, componentId: ComponentId) => {
    let patch : RemoveAttribute = {
        type : "removeAttribute",
        nodeId : node.nodeId,
        key,
        componentId,
    };
    addPatch(componentId, patch);
    return;
  },
  setTextContent : (node: NodeId, text: string, componentId: ComponentId) => {
    let patch : SetTextContent = {
        type : "setTextContent",
        nodeId : node.nodeId,
        componentId,
        text
    };
    addPatch(componentId, patch);
    return;
  },
  flush: (componentId: ComponentId): void => {
    globalThis['patches'] = [];
    return;
  },
  getRoot : function () {
    return { nodeId : 0 };
  },

};
