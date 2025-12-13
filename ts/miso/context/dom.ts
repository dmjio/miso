import
  { CSS
  , DrawingContext
  , EventContext
  , EventCapture
  , ComponentId
  , HydrationContext
  , DOMRef
  , ComponentContext
  , VTree
  } from '../types';

export const eventContext : EventContext<DOMRef> = {
  addEventListener : (mount: DOMRef, event: string, listener, capture: boolean) => {
      mount.addEventListener(event, listener, capture);
  },
  removeEventListener : (mount: DOMRef, event: string, listener, capture: boolean) => {
      mount.removeEventListener(event, listener, capture);
  },
  isEqual: (x: DOMRef, y: DOMRef) : boolean => {
    return x === y;
  },
  getTarget: (e : Event) : DOMRef => {
    return e.target as DOMRef;
  },
  parentNode : (node: DOMRef): DOMRef => {
    return node.parentNode as DOMRef;
  },
};

export const hydrationContext : HydrationContext<DOMRef> = {
  getInlineStyle: (node: DOMRef, key: string) => {
    return node.style[key];
  },
  firstChild : (node: DOMRef) => {
    return node.firstChild as DOMRef;
  },
  lastChild : (node : DOMRef) => {
    return node.lastChild as DOMRef;
  },
  getAttribute: (node: DOMRef, key: string) => {
      if (key === 'class') return node.className;
      if (key in node) return node[key];
      return node.getAttribute(key);
  },
  getTag: (node: DOMRef) => {
    return node.nodeName;
  },
  getTextContent: (node: DOMRef) => {
    return node.textContent;
  },
  children: (node: DOMRef) => {
    return node.childNodes as any;
  },
};

export const componentContext : ComponentContext = {
    mountComponent : function (events: Array<EventCapture>, componentId: ComponentId, model: Object) : void {
        return;
    },
    unmountComponent : function (componentId: ComponentId) : void {
        return;
    },
    modelHydration : function (model: Object) : void {
        return;
    }
};

export const drawingContext : DrawingContext<DOMRef> = {
  nextSibling : (node: VTree<DOMRef>) => {
    return node.domRef.nextSibling as DOMRef;
  },
  createTextNode : (s: string) => {
    return document.createTextNode(s) as any; // dmj: hrm
  },
  createElementNS : (ns: string, tag: string) => {
    return document.createElementNS(ns, tag) as DOMRef;
  },
  appendChild : (parent: DOMRef, child: DOMRef) => {
    return parent.appendChild (child);
  },
  replaceChild : (parent: DOMRef, n: DOMRef, old: DOMRef) => {
    return parent.replaceChild (n, old);
  },
  removeChild : (parent: DOMRef, child: DOMRef) => {
    return parent.removeChild (child);
  },
  createElement : (tag: string) => {
    return document.createElement(tag);
  },
  addClass : (className: string, domRef: DOMRef) => {
    if (className) domRef.classList.add(className);
  },
  removeClass : (className: string, domRef: DOMRef) => {
    if (className) domRef.classList.remove(className);
  },
  insertBefore : (parent: DOMRef, child: DOMRef, node: DOMRef) => {
    return parent.insertBefore(child, node);
  },
  swapDOMRefs : (a: DOMRef, b: DOMRef, p: DOMRef) => {
    // swap positions of siblings a and b under the same parent p
    const nextB = b.nextSibling;
    p.insertBefore(b, a);      // place b before a
    p.insertBefore(a, nextB);  // place a before what was originally after b
    return;
  },
  setInlineStyle: (cCss: CSS, nCss: CSS, node: DOMRef) => {
     var result: string;
     /* is current attribute in new attribute list? */
     for (const key in cCss) {
       result = nCss[key];
       if (!result) {
         /* current key is not in node */
         if (key in node.style) {
           node.style[key] = '';
         } else {
           node.style.setProperty(key, '');
         }
       } else if (result !== cCss[key]) {
         if (key in node.style) {
           node.style[key] = result;
         } else {
           node.style.setProperty(key,result);
         }
       }
     }
     /* add remaining */
     for (const n in nCss) {
       if (cCss && cCss[n]) continue;
       if (n in node.style) {
         node.style[n] = nCss[n];
       } else {
         node.style.setProperty(n,nCss[n]);
       }
     }
    return;
  },
  setAttribute: (node: DOMRef, key: string, value: any) => {
    return node.setAttribute(key, value)
  },
  setAttributeNS: (node: DOMRef, ns: string, key: string, value: any) => {
    return node.setAttributeNS(ns, key, value)
  },
  removeAttribute : (node: DOMRef, key: string) => {
    return node.removeAttribute(key);
  },
  setTextContent : (node: DOMRef, text: string) => {
    node.textContent = text;
    return;
  },
  flush: (): void => {
    return;
  },
  getRoot : function () {
    return document.body
  },
};
