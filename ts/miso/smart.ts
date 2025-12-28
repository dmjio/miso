/* smart constructors for VTree */
import { Mount, VText, VTree, VNode, VComp, VTreeType } from './types';

/* vtext factory */
export function vtext<T>(input: string) : VText<T> {
    return {
      ns : 'text',
      text: input,
      type: VTreeType.VText,
      domRef : null,
      key : null,
      parent : null,
      nextSibling: null,
    };
}

export function vtextKeyed<T>(input: string, key: string) : VText<T> {
    return union(vtext<T>(input), { key });
}

/* vtree factory */
export function vnode<T>(props: Partial<VNode<T>>): VNode<T> {
  var node = union(mkVNode<T>(), props);
  return node;
}

export function vcomp<T>(props: Partial<VComp<T>>): VComp<T> {
  return union(mkVComp<T>(), props);
}

/* set union */
function union<T extends object>(obj: T, updates: Partial<T>): T {
  return Object.assign({}, obj, updates);
}

/* smart constructors */
export function vnodeKeyed<T>(tag:string, key:string): VNode<T> {
  return vnode<T>({
    tag: tag,
    children: [vtext<T>(key)],
    key: key,
  });
}

export function vnodeKids<T>(tag:string, kids:Array<VTree<T>>): VNode<T> {
  return vnode({
    tag: tag,
    children: kids,
  });
}

/* "smart" helper for constructing an empty virtual DOM */
function mkVNode<T>() : VNode<T> {
  return {
    props: {},
    css: {},
    classList: null,
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
    type : VTreeType.VNode,
    nextSibling: null,
    parent : null,
  };
}

function mkVComp<T>() : VComp<T> {
  return union(mkVNode<T>() as any, {
    type : VTreeType.VComp,
    mount: (_parent: T): Mount<T> => ({ componentId: 0, componentTree: mkVNode<T>() }),
    unmount: () => {},
    onUnmounted: () => {},
    onBeforeUnmounted: () => {},
    onMounted: () => {},
    onBeforeMounted: () => {},
    onCreated: () => {},
  });
}

