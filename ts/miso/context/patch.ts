import { DrawingContext, EventContext, NodeId } from '../types';

globalThis['componentId'] = 0;
globalThis['nodeId'] = 0;

export const eventContext : EventContext<NodeId> = {
  addEventListener : (mount, event, listener, capture) => {
      // mount.addEventListener(event, listener, capture);
      // dmj: not used in dom.ts (abstract out into event context)
  },
  removeEventListener : (mount, event, listener, capture) => {
      // mount.removeEventListener(event, listener, capture);
      // dmj: not used in dom.ts (abstract out into event context)
  },
  isEqual: function (x, y) {
    return x.nodeId === y.nodeId;
    // return x === y;
  },
  getTarget: function (e) {
    // return e.target as DOMRef;
    // retrieve the root
    return { nodeId : globalThis['nodeId']++ }
  },
  parentNode: function (node) {
    // return e.target as DOMRef;
    // retrieve the root
    return { nodeId : 10 } //TODO: flesh
  },
}

export const drawingContext : DrawingContext<NodeId> = {
  nextSibling : (node) => {
    // return node.nextSibling;
    // dmj: this is important for use in
    return null; // TODO
  },
  createTextNode : (value) => {
      // return document.createTextNode(s) as any;
      // var next = globalThis['nodeId']++;
      return { // TODO
        nodeId: 0,
        text : value
      };
  },
  createElementNS : (ns, tag) => {
      var next = globalThis['nodeId']++;
      // dmj: use the typescript types here
      // add to patch object for transfer to main thread
      return {
        nodeId: next,
        ns : ns,
        tag: tag,
      };
  },
  appendChild : (parent, child) => {
    // return parent.appendChild (child);
  },
  replaceChild : (parent, n, old) => {
    // return parent.replaceChild (n, old);
  },
  removeChild : (parent, child) => {
    // return parent.removeChild (child);
  },
  createElement : (tag) => {
    // return document.createElement(tag);
      return { nodeId : 10 };
  },
  insertBefore : (parent, child, node) => {
    // return parent.insertBefore(child, node);
  },
  swapDOMRefs : (a, b, p) => {
    // const tmp = a.nextSibling;
    // p.insertBefore(a, b);
    // p.insertBefore(b, tmp);
    // return;
  },
  setInlineStyle: (cCss, nCss, node) => {
    //  var result: string;
    //  /* is current attribute in new attribute list? */
    //  for (const key in cCss) {
    //    result = nCss[key];
    //    if (!result) {
    //      /* current key is not in node */
    //      node.style[key] = '';
    //    } else if (result !== cCss[key]) {
    //        node.style[key] = result;
    //    }
    //  }
    //  /* add remaining */
    //  for (const n in nCss) {
    //    if (cCss && cCss[n]) continue;
    //    node.style[n] = nCss[n];
    //  }
    // return;
  },

  setAttribute: (node, key, value) => {
    // return node.setAttribute(key, value);
  },
  setAttributeNS: (node, ns, key, value) => {
    // return node.setAttributeNS(ns, key, value)
  },
  removeAttribute : (node, key) => {
    // return node.removeAttribute(key);
  },
  setTextContent : (node, text) => {
    // node.textContent = text;
    // return;
  },
  flush: (): void => {
    return;
  },
  getRoot : function () {
    // return document.body
    // retrieve the root
    return { nodeId : 0 /* globalThis['nodeId']++ */ };
  },

};
