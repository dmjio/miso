import { Context, DOMRef } from '../types';

const context : Context<DOMRef> = {
  addEventListener : (mount, event, listener, capture) => {
      mount.addEventListener(event, listener, capture);
  },
  firstChild : (node) => {
    return node.firstChild;
  },
  lastChild : (node) => {
    return node.lastChild;
  },
  parentNode : (node) => {
    return node.parentNode;
  },
  nextSibling : (node) => {
    return node.nextSibling;
  },
  createTextNode : (s) => {
    return document.createTextNode(s) as any;
  },
  createElementNS : (ns, tag) => {
    return document.createElementNS(ns, tag) as any;
  },
  appendChild : (parent, child) => {
    return parent.appendChild (child);
  },
  replaceChild : (parent, n, old) => {
    return parent.replaceChild (n, old);
  },
  removeChild : (parent, child) => {
    return parent.removeChild (child);
  },
  createElement : (tag) => {
    return document.createElement(tag);
  },
  insertBefore : (parent, child, node) => {
    return parent.insertBefore(child, node);
  },
  swapDOMRefs : (a, b, p) => {
    const tmp = a.nextSibling;
    p.insertBefore(a, b);
    p.insertBefore(b, tmp);
    return;
  },
  querySelectorAll: (sel) => {
    return document.querySelectorAll(sel) as any;
  },
  setInlineStyle: (cCss, nCss, node) => {
     var result: string;
     /* is current attribute in new attribute list? */
     for (const key in cCss) {
       result = nCss[key];
       if (!result) {
         /* current key is not in node */
         node.style[key] = '';
       } else if (result !== cCss[key]) {
           node.style[key] = result;
       }
     }
     /* add remaining */
     for (const n in nCss) {
       if (cCss && cCss[n]) continue;
       node.style[n] = nCss[n];
     }
    return;
  },
  getInlineStyle: (node, key) => {
    return node.style[key];
  },
  setAttribute: (node, key, value) => {
    return node.setAttribute(key, value)
  },
  getAttribute: (node, key) => {
      if (key === 'class') return node.className;
      if (key in node) return node[key];
      return node.getAttribute(key);
  },
  setAttributeNS: (node, ns, key, value) => {
    return node.setAttributeNS(ns, key, value)
  },
  removeAttribute : (node, key) => {
    return node.removeAttribute(key);
  },
  setTextContent : (node, text) => {
    node.textContent = text;
    return;
  },
  getTag: (node) => {
    return node.nodeName;
  },
  getTextContent: (node) => {
    return node.textContent;
  },
  children: (node) => {
    return node.childNodes as any;
  },
  isEqual: (x, y) => {
    return x === y;
  },
  getTarget: (e : Event) => {
    return e.target;
  },
  flush: (): void => {
    return;
  },
  getRoot : () => {
    return document.body
  },
};

export {
  context
};
