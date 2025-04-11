/* smart constructors for VTree */
import { VText, VTree, VNode, VComp } from './types';

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
  return union(mkVNode(), props);
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
    type : 'vnode',
  };
}

function mkVComp() : VComp {
  return union(mkVNode() as any, {
    type : 'vcomp',
    'data-component-id': '',
    mount: () => {},
    unmount: () => {},
    onCreated: () => {},
  });
}

