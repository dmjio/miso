/* core type for virtual DOM */
export type Props = Record<string, string>;
export type CSS = Record<string, string>;
export type Events<T> = Record<string, EventObject<T>>;

/* element name spacing */
export type NS = 'text' | 'html' | 'svg' | 'mathml';

export type DOMRef = HTMLElement | MathMLElement | SVGElement;
export type ComponentId = number;

export type VComp<T> = {
  type: 'vcomp';
  domRef: T;
  ns: 'html';
  tag: string;
  key: string;
  props: Props;
  css: CSS;
  events: Events<T>;
  children: Array<VTree<T>>;
  onBeforeMounted: () => void;
  onMounted: (domRef: T) => void;
  onBeforeUnmounted: () => void;
  onUnmounted: (domRef: T) => void;
  mount: (domRef: T, callback: ((componentId : ComponentId, component: VTree<T>) => void)) => void;
  unmount: (e: T) => void;
};

export type VNode<T> = {
  type: 'vnode';
  ns: NS;
  domRef: T;
  tag: string;
  key: string;
  props: Props;
  css: CSS;
  events: Events<T>;
  shouldSync: boolean;
  children: Array<VTree<T>>;
  onDestroyed: () => void;
  onBeforeDestroyed: () => void;
  onCreated: () => void;
  onBeforeCreated: () => void;
  draw?: (T) => void;
};

export type VText<T> = {
  type: 'vtext';
  text: string;
  domRef: T;
  ns: NS;
  key: string;
};

export type NodeId = {
  nodeId: number;
}

export type VTree<T> = VComp<T> | VNode<T> | VText<T>;

export type EventObject<T> = {
  options: Options;
  runEvent: (e: Event, node: T) => void;
};

export type Options = {
  preventDefault: boolean;
  stopPropagation: boolean;
};

export type EventCapture = {
  name: string;
  capture: boolean;
};

/*
  dmj: Context used for dependency injection of native or browser environment.
*/
export type Context<T> = {
  addEventListener : (mount : T, event : string, listener : any, capture : boolean) => void;
  removeEventListener : (mount : T, event : string, listener : any, capture : boolean) => void;
  nextSibling : (node) => T;
  createTextNode : (s: string) => T;
  createElementNS : (ns : string, tag : string) => T;
  appendChild : (parent : T, child : T) => void;
  replaceChild : (parent : T, n : T, o : T) => void;
  removeChild : (parent, child) => void;
  createElement : (name : string) => T;
  insertBefore : (parent : T, child, node) => void;
  swapDOMRefs: (a: T, b: T, p: T) => void;
  querySelectorAll: (sel: string) => Array<T>;
  setAttribute : (node, key, value) => void;
  removeAttribute : (node, key) => void;
  setAttributeNS : (node, ns, key, value) => void;
  setTextContent : (node, text) => void;
  getTextContent : (node) => string;
  isEqual : (n1, n2) => boolean;
  getTarget : (e: Event) => T;
  children : (e: T) => Array<T>;
  getInlineStyle : (e, string) => string;
  setInlineStyle : (cCss: CSS, nCss: CSS, node : T) => void;
  getAttribute : (e: T, string) => string;
  getTag : (e) => string;
  firstChild : (e) => T;
  lastChild : (e) => T;
  parentNode : (e) => T;
  flush : () => void;
  getRoot : () => T;
};

/* dmj: used for Fetch API */
export type Response = {
  body: any;
  status: number;
  headers: Record<string,string>;
  error: string;
};
