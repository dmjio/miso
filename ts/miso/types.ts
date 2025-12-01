/* core type for virtual DOM */
export type Props = Record<string, any>;
export type CSS = Record<string, string>;
export type Events<T> = Record<string, EventObject<T>>;

/* element name spacing */
export type NS = 'text' | 'html' | 'svg' | 'mathml';

export type DOMRef = HTMLElement | MathMLElement | SVGElement;
export type ComponentId = number;

/* dmj: drop the unneeded fields */
export type VComp<T> = {
  type: 'vcomp';
  domRef: T;
  ns: 'html';
  tag: string;
  key: string;
  props: Props;
  css: CSS;
  events: Events<T>;
  parent: Parent<T>;
  children: Array<VTree<T>>;
  propagateEvents: boolean;

  /* hoist put these into Haskell Component */
  onBeforeMounted: () => void;
  onMounted: (domRef: T) => void;
  onBeforeUnmounted: (domRef: T) => void;
  onUnmounted: (domRef: T) => void;
  mount: (domRef: T, callback: ((componentId : ComponentId, component: VTree<T>) => void)) => void;
  unmount: (e: T) => void;

  nextSibling: VNode<T>;
  componentId: number;
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
  children: Array<VTree<T>>;
  onDestroyed: () => void;
  onBeforeDestroyed: () => void;
  onCreated: () => void;
  onBeforeCreated: () => void;
  draw?: (T) => void;
  parent: Parent<T>;
  nextSibling: VNode<T>;
};

export type VText<T> = {
  type: 'vtext';
  text: string;
  domRef: T;
  ns: NS;
  key: string;
  parent: Parent<T>;
};

export type Parent<T> = VNode<T> | VComp<T>;

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
  mountComponent : (events: Array<EventCapture>, componentId: ComponentId, model: Object) => void,
  unmountComponent : (componentId: ComponentId) => void,
  modelHydration : (componentId: ComponentId, model: Object) => void
}

/* dmj: TODO: parent should go in here as well */
export type DrawingContext<T> = {
  nextSibling : (node: VNode<T>) => T;
  createTextNode : (s: string) => T;
  createElementNS : (ns: string, tag : string) => T;
  appendChild : (parent: T, child: T) => void;
  replaceChild : (parent: T, n: T, o: T) => void;
  removeChild : (parent: T, child: T) => void;
  createElement : (name: string) => T;
  insertBefore : (parent: T, child: T, node: T) => void;
  swapDOMRefs: (a: T, b: T, p: T) => void;
  setAttribute : (node: T, key: string, value : any) => void;
  removeAttribute : (node: T, key :string) => void;
  setAttributeNS : (node: T, ns: string, key: string, value: any) => void;
  setTextContent : (node: T, text: string) => void;
  setInlineStyle : (cCss: CSS, nCss: CSS, node : T) => void;
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
