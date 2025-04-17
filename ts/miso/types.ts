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
  onMounted: () => void;
  onBeforeUnmounted: () => void;
  onUnmounted: () => void;
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
  runEvent: (e: Event) => void;
};

type Options = {
  preventDefault: boolean;
  stopPropagation: boolean;
};

type EventCapture = {
  name: string;
  capture: boolean;
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
};
