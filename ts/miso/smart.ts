/* smart constructors for VTree */
import { VTree, VNode, VComp } from './types';

/* vtext factory */
export function vtext(input: string) {
  return vtree({ text: input, type: 'vtext' });
}

export function vtextKeyed(input: string, k: string) {
  return vtree({ text: input, type: 'vtext', key: k });
}

/* vtree factory */
export function vtree(props: any): VNode {
  return !props ? mkVTree() : union(mkVTree(), props);
}

export function vcomp(props: any): VComp {
  return !props ? mkVComp() : union(mkVComp(), props);
}

/* set union */
function union<T extends object>(obj: T, updates: Partial<T>): T {
  return Object.assign({}, obj, updates);
}

/* smart constructors */
export function vnodeKeyed(tag:string, key:string): VTree {
  return vtree({
    type: 'vnode',
    tag: tag,
    children: [vtext(key)],
    key: key,
  });
}

export function vnodeKids(tag:string, kids:Array<VTree>): VTree {
  return vtree({
    type: 'vnode',
    tag: tag,
    children: kids,
  });
}

/* "smart" helper for constructing an empty virtual DOM */
export function mkVTree(): VNode {
  return {
    props: {},
    css: {},
    children: [],
    ns: 'html',
    type: 'vnode',
    domRef: null,
    tag: 'div',
    key: null,
    events: {},
    onDestroyed: () => {},
    onBeforeDestroyed: () => {},
    onCreated: () => {},
  };
}

export function mkVComp(): VComp {
  return {
    props: {},
    css: {},
    children: [],
    ns: 'html',
    type: 'vcomp',
    domRef: null,
    tag: 'div',
    key: null,
    events: {},
    'data-component-id': '',
    onDestroyed: () => {},
    onBeforeDestroyed: () => {},
    mount: () => {},
    unmount: () => {},
    onCreated: () => {},
  };
}
