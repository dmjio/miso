/* smart constructors for VTree */
import { VTree } from './types';

/* vtext factory */
export function vtext(input: string) {
  return vtree({ text: input, type: 'vtext' });
}

export function vtextKeyed(input: string, k: string) {
  return vtree({ text: input, type: 'vtext', key: k });
}

/* vtree factory */
export function vtree(props: any): VTree {
  return !props ? mkVTree() : union(mkVTree(), props);
}

/* set union */
function union<T extends object>(obj: T, updates: Partial<T>): T {
  return Object.assign({}, obj, updates);
}

/* smart constructors */
export function vnodeKeyed(tag, key): VTree {
  return vtree({
    type: 'vnode',
    tag: tag,
    children: [vtext(key)],
    key: key,
  });
}

export function vnodeKids(tag, kids): VTree {
  return vtree({
    type: 'vnode',
    tag: tag,
    children: kids,
  });
}

/* "smart" helper for constructing an empty virtual DOM */
export function mkVTree(): VTree {
  return {
    props: null,
    css: null,
    children: [],
    ns: 'html',
    type: 'vnode',
    domRef: null,
    tag: 'div',
    key: null,
    text: '',
    events: null,
    'data-component-id': null,
    onDestroyed: () => {},
    onBeforeDestroyed: () => {},
    mount: () => {},
    unmount: () => {},
    onCreated: () => {},
  };
}
