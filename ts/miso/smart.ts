/* smart constructors for VTree */
import { VText, VTree, VNode, VComp } from './types';
import { shouldSync } from './util';

/* vtext factory */
export function vtext(input: string) : VText {
    return {
      ns : 'text',
      text: input,
      type: 'vtext',
      domRef : null,
      key : null,
    };
}

export function vtextKeyed(input: string, key: string) : VText {
    return union(vtext(input), { key });
}

/* vtree factory */
export function vnode(props: Partial<VNode>): VNode {
  var node = union(mkVNode(), props);
  /* dmj: If the property is already set the check is bypassed.
     By setting 'shouldSync' manually in  'vnode' you are implicitly
     saying all keys exist and should be synched.
   */
  if (!node['shouldSync']) node['shouldSync'] = shouldSync(node);
  return node;
}

export function vcomp(props: Partial<VComp>): VComp {
  return union(mkVComp(), props);
}

/* set union */
function union<T extends object>(obj: T, updates: Partial<T>): T {
  return Object.assign({}, obj, updates);
}

/* smart constructors */
export function vnodeKeyed(tag:string, key:string): VNode {
  return vnode({
    tag: tag,
    children: [vtext(key)],
    key: key,
  });
}

export function vnodeKids(tag:string, kids:Array<VTree>): VNode {
  return vnode({
    tag: tag,
    children: kids,
  });
}

/* "smart" helper for constructing an empty virtual DOM */
function mkVNode() : VNode {
  return {
    props: {},
    css: {},
    children: [],
    ns: 'html',
    domRef: null,
    tag: 'div',
    key: null,
    events: {},
    onDestroyed: () => {},
    onBeforeDestroyed: () => {},
    onCreated: () => {},
    onBeforeCreated: () => {},
    shouldSync: false,
    type : 'vnode',
  };
}

function mkVComp() : VComp {
  return union(mkVNode() as any, {
    type : 'vcomp',
    'component-id': '',
    mount: () => {},
    unmount: () => {},
    onUnmounted: () => {},
    onBeforeUnmounted: () => {},
    onMounted: () => {},
    onBeforeMounted: () => {},
    onCreated: () => {},
  });
}

