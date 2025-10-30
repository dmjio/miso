import { NodeId, Context } from '../types';

globalThis['componentId'] = 0;
globalThis['nodeId'] = 0;

const context : Context<NodeId> = {
  addEventListener : (mount, event, listener, capture) => {
      // mount.addEventListener(event, listener, capture);
      // dmj: not used in dom.ts (abstract out into event context)
  },
  removeEventListener : (mount, event, listener, capture) => {
      // mount.removeEventListener(event, listener, capture);
      // dmj: not used in dom.ts (abstract out into event context)
  },
  firstChild : (node) => {
      // return node.firstChild;
      // dmj: not used in dom.ts (abstract out into hydration context)
      return { nodeId: 0 }; // TODO
  },
  lastChild : (node) => {
      // return node.lastChild;
      // dmj: not used in dom.ts (abstract out into hydration context)
      return { nodeId: 0 }; // TODO
  },
  parentNode : (node) => {
      // return node.parentNode;
      // dmj: not used in dom.ts (abstract out into event context)
      return { nodeId: 0 }; // TODO
  },
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
  querySelectorAll: (sel) => {
    // return document.querySelectorAll(sel) as any;
    return [];
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
  getInlineStyle: (node, key) => {
    // return node.style[key];
      return "foo";
  },
  setAttribute: (node, key, value) => {
    // return node.setAttribute(key, value);
  },
  getAttribute: (node, key) => {
    // if (key === 'class') return node.className;
    // if (key in node) return node[key];
    // return node.getAttribute(key);
      return "TODO";
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
  getTag: (node) => {
    // return node.nodeName;
    return "TODO";
  },
  getTextContent: function (node) {
    // return node.textContent;
    return "TODO";
  },
  children: function (node) {
      // return node.childNodes as any;
      return [];
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
  flush: (): void => {
    return;
  },
  getRoot : function () {
    // return document.body
    // retrieve the root
    return { nodeId : 0 /* globalThis['nodeId']++ */ };
  },

};

export {
  context
};
