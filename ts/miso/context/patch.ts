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

  This file is to setup a custom context for use w/ preact-based setup (like Lynx requires).
  We used this in patch.spec.ts to test patch accumulation + application in the diff.

  This is used to provide testing for miso-lynx, or other patch-based architectures
  that us miso's diff.

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
  MountComponent,
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
    mountComponent : function (events: Array<EventCapture>, componentId: ComponentId, model: Object) {
        let patch : MountComponent = {
            type: "mount",
            componentId: componentId,
            mountPoint : 0,
            events,
            model
        };
        addPatch(componentId, patch);
        return;
    },
    unmountComponent : function (componentId: ComponentId) {
        let patch : UnmountComponent = {
            type: "unmount",
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
        return;
    }
};

export const patchDrawingContext : DrawingContext<NodeId> = {
  nextSibling : (node: VNode<NodeId>) => {
    return node.nextSibling.domRef;
  },
  createTextNode : (value : string) => {
      const nodeId: number = nextNodeId ();
      const componentId: number = getComponentId ();
      let patch : CreateTextNode = {
          text : value,
          type : "createTextNode",
          nodeId,
          componentId
      };
      addPatch(componentId, patch);
      return { nodeId };
  },
  createElementNS : (ns: string, tag: string) => {
    const nodeId: number = nextNodeId ();
    const componentId: number = getComponentId ();
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
  appendChild : (parent: NodeId, child: NodeId) => {
    const componentId: number = getComponentId ();
    let patch : AppendChild = {
        type: "appendChild",
        parent: parent.nodeId,
        child: child.nodeId,
        componentId
    };
    addPatch(componentId, patch);
    return;
  },
  replaceChild : (parent: NodeId, n: NodeId, old: NodeId) => {
    const componentId: number = getComponentId ();
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
  removeChild : (parent: NodeId, child: NodeId) => {
    const componentId: number = getComponentId ();
    let patch : RemoveChild = {
        type: "removeChild",
        parent: parent.nodeId,
        child: child.nodeId,
        componentId
    };
    addPatch(componentId, patch);
    return;
  },
  createElement : (tag) => {
    const nodeId: number = nextNodeId ();
    const componentId: number = getComponentId ();
    let patch : CreateElement = {
        type : "createElement",
        nodeId,
        componentId,
        tag
    };
    addPatch(componentId, patch);
    return { nodeId };
  },
  insertBefore : (parent: NodeId, node: NodeId, child: NodeId) => {
    const componentId: number = getComponentId ();
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
  swapDOMRefs : (a: NodeId, b: NodeId, p: NodeId) => {
    const componentId: number = getComponentId ();
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
  setInlineStyle: (cCss: CSS, nCss: CSS, node: NodeId) => {
    if (areEqual(cCss, nCss)) return;
    const componentId: number = getComponentId ();
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
  setAttribute: (node, key, value) => {
    const componentId: number = getComponentId ();
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
  setAttributeNS: (node, namespace, key, value) => {
    const componentId: number = getComponentId ();
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
  removeAttribute : (node, key) => {
    const componentId: number = getComponentId ();
    let patch : RemoveAttribute = {
        type : "removeAttribute",
        nodeId : node.nodeId,
        key,
        componentId,
    };
    addPatch(componentId, patch);
    return;
  },
  setTextContent : (node: NodeId, text: string) => {
    const componentId: number = getComponentId ();
    let patch : SetTextContent = {
        type : "setTextContent",
        nodeId : node.nodeId,
        componentId,
        text
    };
    addPatch(componentId, patch);
    return;
  },
  flush: (): void => {
    globalThis['patches'] = [];
    return;
  },
  getRoot : function () {
    return { nodeId : 0 };
  },

};
