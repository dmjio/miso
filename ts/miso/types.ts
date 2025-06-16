/* core type for virtual DOM */
type Props = Record<string, string>;
type CSS = Record<string, string>;
type Events = Record<string, EventObject>;

/* element name spacing */
type NS = 'text' | 'html' | 'svg' | 'mathml';

type DOMRef = HTMLElement | SVGElement | MathMLElement;

type VComp = {
  type: 'vcomp';
  domRef: HTMLElement;
  ns: 'html';
  tag: 'div';
  key: string;
  props: Props;
  css: CSS;
  events: Events;
  'data-component-id': string;
  children: Array<VTree>;
  onBeforeMounted: () => void;
  onMounted: (componentId: string) => void;
  onBeforeUnmounted: () => void;
  onUnmounted: (componentId: string) => void;
  mount: (f: (component: VTree) => void) => void;
  unmount: (e: Element) => void;
};

type VNode = {
  type: 'vnode';
  ns: NS;
  domRef: DOMRef;
  tag: string;
  key: string;
  props: Props;
  css: CSS;
  events: Events;
  shouldSync: boolean;
  children: Array<VTree>;
  onDestroyed: () => void;
  onBeforeDestroyed: () => void;
  onCreated: () => void;
  onBeforeCreated: () => void;
  draw?: (DOMRef) => void;
};

type VText = {
  type: 'vtext';
  text: string;
  domRef: Text;
  ns: NS;
  key: string;
};

type VTree = VComp | VNode | VText;

type EventObject = {
  options: Options;
  runEvent: (e: Event, node : DOMRef) => void;
};

type Options = {
  preventDefault: boolean;
  stopPropagation: boolean;
};

type EventCapture = {
  name: string;
  capture: boolean;
};

/*
  dmj: Context used for dependency injection of native or browser environment.
*/
type Context = {
  addEventListener : (mount : Node, event : string, listener : any, capture : boolean) => void;
  createTextNode : (s: string) => Text;
  createElementNS : (ns : string, tag : string) => Element;
  appendChild : (parent, child) => void;
  replaceChild : (parent, n, o) => void;
  removeChild : (parent, child) => void;
  createElement : (name : string) => Element;
  insertBefore : (parent, child, node) => void;
  swapDOMRefs: (a: Node, b: Node, p: Node) => void;
  querySelectorAll: (sel: string) => NodeListOf<Element>;
  setAttribute : (node, key, value) => void;
  removeAttribute : (node, key) => void;
  setAttributeNS : (node, ns, key, value) => void;
  setTextContent : (node, text) => void;
  getTextContent : (node) => string;
  isEqual : (n1, n2) => boolean;
  getTarget : (e: Event) => EventTarget;
  setComponentId : (componentId: string) => void;
  children : (e: Node) => NodeListOf<ChildNode>;
  getInlineStyle : (e, string) => string;
  setInlineStyle : (cCss: CSS, nCss: CSS, node : DOMRef) => void;
  getAttribute : (e: Element, string) => string;
  getTag : (e) => string;
  firstChild : (e) => Element;
  lastChild : (e) => Element;
  parentNode : (e) => Element;
  requestAnimationFrame : (callback: ((timestamp: number) => void)) => void;
  flush : () => void;
  getRoot : () => Element;
};

export {
  VTree,
  VComp,
  VNode,
  VText,
  EventCapture,
  EventObject,
  Options,
  Props,
  CSS,
  Events,
  NS,
  DOMRef,
  Context,
};
