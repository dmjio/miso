/* core type for virtual DOM */
export type Props = Record<string, any>;
export type CSS = Record<string, string>;
export type Class = Set<string>;
export type Events<T> = Record<string, Record<string, EventObject<T>>>;

/* element name spacing */
export type NS = 'text' | 'html' | 'svg' | 'mathml';

export type DOMRef = HTMLElement | MathMLElement | SVGElement;
export type ComponentId = number;

export enum VTreeType {
  VComp = 0,
  VNode = 1,
  VText = 2
}

export enum OP {
  APPEND = 0,
  REPLACE = 1,
  INSERT_BEFORE = 2
}

export type VComp<T> = {
  type: VTreeType.VComp;
  child: VTree<T>;
  // used w/ drill to get domRef.
  componentId?: ComponentId;
  // ^ set post-mounting
  key: string;

  parent: Parent<T>;
  nextSibling: VTree<T>;
  eventPropagation: boolean;
  mount: (parent: T) => Mount<T>;
  unmount: (vcompId: ComponentId) => void;
};

export type Mount<T> = {
  componentId: ComponentId;
  componentTree: VTree<T>;
};

export type VNode<T> = {
  type: VTreeType.VNode;
  ns: NS;
  domRef: T;
  tag: string;
  key: string;
  props: Props;
  css: CSS;
  classList: Class;
  events: Events<T>;
  children: Array<VTree<T>>;
  onDestroyed: () => void;
  onBeforeDestroyed: () => void;
  onCreated: (domRef: T) => void;
  onBeforeCreated: () => void;
  draw?: (T) => void;
  parent: Parent<T>;
  nextSibling: VTree<T>;
};

export type VText<T> = {
  type: VTreeType.VText;
  text: string;
  domRef: T;
  ns: NS;
  key: string;
  parent: Parent<T>;
  nextSibling: VTree<T>;
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
  isEqual : (n1: T, n2: T) => boolean;
  getTarget : (e: Event) => T;
  parentNode : (node: T) => T;
  delegator : (mount: T,
    events: Array<EventCapture>,
    getVTree: ((vtree : VTree<T>) => void),
    debug: boolean,
    context: EventContext<T>) => void;
};

export type HydrationContext<T> = {
  getTextContent : (node) => string;
  children : (e: T) => Array<T>;
  getInlineStyle : (e, string) => string;
  getTag : (e: T) => string;
  firstChild : (node: T) => T;
  lastChild : (node: T) => T;
  getAttribute : (node: T, string) => string;
};

export type PRNG = (() => (number));

export type ComponentContext = {
  mountComponent : (events: Array<EventCapture>, componentId: ComponentId, model: Object) => void,
  unmountComponent : (componentId: ComponentId) => void,
  modelHydration : (componentId: ComponentId, model: Object) => void
}

export type DrawingContext<T> = {
  nextSibling : (node: VTree<T>) => T;
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
  addClass : (c: string, domRef: T) => void;
  removeClass : (c: string, domRef: T) => void;
  flush : () => void;
  /** @since 1.9.0.0 */
  getHead : () => T;
  getRoot : () => T;
};

/* dmj: used for Fetch API */
export type Response = {
  body: any;
  status: number;
  headers: Record<string,string>;
  error: string;
};

/* Information about the current component that lives on the render thread */
export type Component = {
  model : Object,
  /* updated model sent from bg to main */
  mainThreadEvents : Record <string, ((o:Object) => void)>,
  /* dmj: these will need to be sent from bg to main on app load*/
  rootId : number,
  /* the root node to diff from */
}

export type Runtime<T> = {
  nodes : Record <number, T>,
  components : Record <ComponentId, Component>
};

/* Convenience table to allow O(1) application of DOM references */
export type NodeMap<T> = Record <number, T>;

/* Message protocol for bidirectional synchronization between MTS / BTS */
export type PATCH
  = InsertBefore
  | SwapDOMRefs
  | CreateElement
  | CreateElementNS
  | CreateTextNode
  | SetAttribute
  | SetAttributeNS
  | AppendChild
  | RemoveChild
  | ReplaceChild
  | RemoveAttribute
  | SetTextContent
  | SetInlineStyle
  | MountComponent
  | UnmountComponent
  | ModelHydration
  | AddClass
  | RemoveClass
  | ProcessEvent
  | AddEventListeners
  | Flush;

export type AddEventListeners = {
  events: Array<EventCapture>,
  type: "addEventListeners"
};

export type ProcessEvent = {
  stack: Array<number>,
  event: Object, /* Event object, will be JSON'ified */
  type: "processEvent"
};

export type ModelHydration = {
  componentId: ComponentId,
  model: Object,
  type: "modelHydration"
};

export type MountComponent = {
  type: "mount",
  componentId: ComponentId,
  model: Object,
  mountPoint: number
};

export type UnmountComponent = {
  type: "unmount",
  componentId: ComponentId,
};

export type InsertBefore = {
  type: "insertBefore",
  parent: number,
  child: number,
  node: number
};

export type SwapDOMRefs = {
  nodeA: number,
  nodeB: number,
  parent: number,
  type: "swapDOMRefs"
};

export type CreateElement = {
  nodeId: number,
  tag: string,
  type: "createElement"
};

export type CreateElementNS = {
  nodeId: number,
  tag: string,
  namespace: string,
  type: "createElementNS"
};

export type CreateTextNode = {
  nodeId: number,
  text: string,
  type: "createTextNode"
};

export type SetAttribute = {
  key: string,
  value: any,
  nodeId: number,
  type: "setAttribute"
};

export type SetAttributeNS = {
  key: string,
  value: any,
  nodeId: number,
  type: "setAttributeNS",
  namespace: string,
};

export type AppendChild = {
  parent: number,
  child: number,
  type: "appendChild"
};

export type ReplaceChild = {
  current: number,
  new: number,
  parent: number,
  type: "replaceChild"
};

export type RemoveChild = {
  parent: number,
  child: number,
  type: "removeChild"
};

export type RemoveAttribute = {
  type: "removeAttribute",
  nodeId: number,
  key: string,
};

export type RemoveClass = {
  type: "removeClass",
  nodeId: number,
  key: string,
};

export type AddClass = {
  type: "addClass",
  nodeId: number,
  key: string,
};

export type SetTextContent = {
  type: "setTextContent",
  nodeId: number,
  text: string,
};

export type SetInlineStyle = {
  new : Record<string, any>,
  current : Record<string, any>,
  nodeId: number,
  type: "setInlineStyle"
};

export type Flush = {
  type: "flush"
};
