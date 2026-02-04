import {
  ComponentId,
  EventCapture,
  DrawingContext,
  NodeId,
  CSS,
  ComponentContext,
  VTree,
  VTreeType,
} from '../types';

import { drill } from '../util';

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
  MountComponent,
  UnmountComponent,
  ModelHydration,
  AddClass,
  RemoveClass,
} from '../patch';

export function addPatch (patch : PATCH) : void {
  globalThis['patches'].push(patch);
}

export function getPatches () : Array<PATCH> {
  return globalThis['patches'];
}

export function nextNodeId () : number {
  return globalThis['nodeId']++;
}

// dmj: Helper for Object equality.
function areEqual(a: Object, b: Object) : boolean {
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
            model
        };
        addPatch(patch);
        return;
    },
    unmountComponent : function (componentId: ComponentId) {
        let patch : UnmountComponent = {
            type: "unmount",
            componentId,
        };
        addPatch(patch);
        return;
    },
    modelHydration : function (componentId: ComponentId, model: Object) {
        let patch : ModelHydration = {
            type: "modelHydration",
            model,
            componentId
        };
        addPatch(patch);
        return;
    }
};

export const patchDrawingContext : DrawingContext<NodeId> = {
  nextSibling : (node: VTree<NodeId>) => {
    if (node.nextSibling) {
      switch (node.nextSibling.type) {
        case VTreeType.VComp:
          const drilled = drill (node.nextSibling);
          return drilled ? drilled : null;
        default:
          return node.nextSibling.domRef as NodeId;
      }
    }
  },
  createTextNode : (value : string) => {
      const nodeId: number = nextNodeId ();
      let patch : CreateTextNode = {
          text : value,
          type : "createTextNode",
          nodeId,
      };
      addPatch(patch);
      return { nodeId };
  },
  createElementNS : (ns: string, tag: string) => {
    const nodeId: number = nextNodeId ();
    let patch : CreateElementNS = {
        type : "createElementNS",
        namespace: ns,
        nodeId,
        tag
    };
    addPatch(patch);
    return { nodeId };
  },
  appendChild : (parent: NodeId, child: NodeId) => {
    let patch : AppendChild = {
        type: "appendChild",
        parent : parent.nodeId,
        child : child.nodeId,
    };
    addPatch(patch);
    return;
  },
  replaceChild : (parent: NodeId, n: NodeId, current: NodeId) => {
    let patch : ReplaceChild = {
        type: "replaceChild",
        parent : parent.nodeId,
        new : n.nodeId,
        current : current.nodeId
    };
    addPatch(patch);
    return;
  },
  removeChild : (parent: NodeId, child: NodeId) => {
    let patch : RemoveChild = {
        type: "removeChild",
        parent : parent.nodeId,
        child : child.nodeId,
    };
    addPatch(patch);
    return;
  },
  createElement : (tag) => {
    const nodeId: number = nextNodeId ();
    let patch : CreateElement = {
        type : "createElement",
        nodeId,
        tag
    };
    addPatch(patch);
    return { nodeId };
  },
  insertBefore : (parent: NodeId, node: NodeId, child: NodeId) => {
    let patch : InsertBefore = {
        type: "insertBefore",
        parent : parent.nodeId,
        child : child.nodeId,
        node : node.nodeId,
    };
    addPatch(patch);
    return;
  },
  swapDOMRefs : (nodeA: NodeId, nodeB: NodeId, parent: NodeId) => {
    let patch : SwapDOMRefs = {
        type: "swapDOMRefs",
        parent : parent.nodeId,
        nodeA : nodeA.nodeId,
        nodeB : nodeB.nodeId,
    };
    addPatch(patch);
    return;
  },
  setInlineStyle: (cCss: CSS, nCss: CSS, n: NodeId) => {
    if (areEqual(cCss, nCss)) return;
    let patch : SetInlineStyle = {
        type : "setInlineStyle",
        nodeId : n.nodeId,
        new: nCss,
        current: cCss,
    };
    addPatch(patch);
    return;
  },
  setAttribute: (n, key, value) => {
    let patch : SetAttribute = {
        type : "setAttribute",
        nodeId : n.nodeId,
        key,
        value,
    };
    addPatch(patch);
    return;
  },
  setAttributeNS: (n, namespace, key, value) => {
    let patch : SetAttributeNS = {
        type : "setAttributeNS",
        nodeId : n.nodeId,
        key,
        value,
        namespace,
    };
    addPatch(patch);
    return;
  },
  removeAttribute : (n, key) => {
    let patch : RemoveAttribute = {
        type : "removeAttribute",
        nodeId : n.nodeId,
        key,
    };
    addPatch(patch);
    return;
  },
  addClass : (key, n) => {
    let patch : AddClass = {
        type : "addClass",
        nodeId : n.nodeId,
        key
    };
    addPatch(patch);
    return;
  },
  removeClass : (key, n) => {
    let patch : RemoveClass = {
        type : "removeClass",
        nodeId : n.nodeId,
        key
    };
    addPatch(patch);
    return;
  },
  setTextContent : (n: NodeId, text: string) => {
    let patch : SetTextContent = {
        type : "setTextContent",
        nodeId : n.nodeId,
        text
    };
    addPatch(patch);
    return;
  },
  flush: (): void => {
    globalThis['patches'] = [];
    return;
  },
  /** @since 1.9.0.0 */
  getHead : function () {
    return { nodeId : 0 };
  },
  getRoot : function () {
    return { nodeId : 0 };
  },

};
