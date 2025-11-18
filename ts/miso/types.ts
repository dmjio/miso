/* core type for virtual DOM */
export type Props = Record<string, any>;
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
  mount: (domRef: T, callback: ((events: Record<string, boolean>, componentId : ComponentId, component: VTree<T>) => void)) => void;
  unmount: (e: T) => void;
  nextSibling: VNode<T>;
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
  nextSibling: VNode<T>;
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
  This is used to abstract event delegation, hydration and DOM diffing over a generic T.
*/

export type EventContext<T> = {
  addEventListener : (mount : T, event : string, listener : any, capture : boolean) => void;
  removeEventListener : (mount : T, event : string, listener : any, capture : boolean) => void;
  isEqual : (n1: T, n2: T) => boolean;
  getTarget : (e: Event) => T;
  parentNode : (node: T) => T;
}

export type HydrationContext<T> = {
  getTextContent : (node) => string;
  children : (e: T) => Array<T>;
  getInlineStyle : (e, string) => string;
  getTag : (e: T) => string;
  firstChild : (node: T) => T;
  lastChild : (node: T) => T;
  getAttribute : (node: T, string) => string;
};

export type ComponentContext = {
  unmountComponent : (componentId: ComponentId) => void,
  modelHydration : (componentId: ComponentId, model: Object) => void
}

export type DrawingContext<T> = {
  mountComponent : (events: Record<string, boolean>, componentId: ComponentId, model: Object) => void,
  nextSibling : (node: VNode<T>) => T;
  createTextNode : (s: string, componentId: ComponentId) => T;
  createElementNS : (ns: string, tag : string, componentId: ComponentId) => T;
  appendChild : (parent: T, child: T, parentComponentId: ComponentId) => void;
  replaceChild : (parent: T, n: T, o: T, componentId: ComponentId) => void;
  removeChild : (parent: T, child: T, componentId: ComponentId) => void;
  createElement : (name: string, componentId: ComponentId) => T;
  insertBefore : (parent: T, child: T, node: T, componentId: ComponentId) => void;
  swapDOMRefs: (a: T, b: T, p: T, componentId: ComponentId) => void;
  setAttribute : (node: T, key: string, value : any, componentId: ComponentId) => void;
  removeAttribute : (node: T, key: string, componentId: ComponentId) => void;
  setAttributeNS : (node: T, ns: string, key: string, value: any, componentId: ComponentId) => void;
  setTextContent : (node: T, text: string, componentId: ComponentId) => void;
  setInlineStyle : (cCss: CSS, nCss: CSS, node : T, componentId: ComponentId) => void;
  flush : (componentId: ComponentId) => void;
  getRoot : () => T;
};

/* dmj: used for Fetch API */
export type Response = {
  body: any;
  status: number;
  headers: Record<string,string>;
  error: string;
};
