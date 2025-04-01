/* core type for virtual DOM */
type Props = Record<string, string>;
type CSS = Record<string, string>;
type Events = Record<string, EventObject>;

type VTree = {
  type: 'vtext' | 'vnode' | 'vcomp';
  ns: 'html' | 'svg' | 'mathml';
  domRef: any;
  text: string;
  tag: string;
  key: string;
  props: Props;
  css: CSS;
  events: Events;
  'data-component-id': string;
  children: Array<VTree>;
  onDestroyed: () => void;
  onCreated: () => void;
  onBeforeDestroyed: () => void;
  mount: (VTree) => void;
  unmount: (Element) => void;
};

type EventObject = {
  options: Options;
  runEvent: (Event) => void;
};

type Options = {
  preventDefault: boolean;
  stopPropagation: boolean;
};

type EventCapture = {
  name: string;
  capture: boolean;
};

export { VTree, EventCapture, EventObject, Options, Props, CSS, Events };
