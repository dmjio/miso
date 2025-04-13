import { DOMRef, VComp, VTree, Props, CSS } from './types';
import { vnode } from './smart';

/* virtual-dom diffing algorithm, applies patches as detected */
export function diff(currentObj: VTree, newObj: VTree, parent: Element): void {
  if (!currentObj && !newObj) return;
  else if (!currentObj && newObj) create(newObj, parent);
  else if (!newObj) destroy(currentObj, parent);
  else {
    if (currentObj['type'] === newObj['type']) {
      diffNodes(currentObj, newObj, parent);
    } else {
      replace(currentObj, newObj, parent);
    }
  }
}
// replace everything function
function replace(c: VTree, n: VTree, parent: Element): void {
  // step1 : prepare to delete, unmount things
  callBeforeDestroyedRecursive(c);
  // ^ this will unmount sub components before we replace the child
  // and will recursively call hooks on nodes
  // step2 : create new things, replace old things with new things
  if (n['type'] === 'vtext') {
    n['domRef'] = document.createTextNode(n['text']);
    parent.replaceChild(n['domRef'], c['domRef']);
  } else {
    createElement(n, (ref: Element) => {
      parent.replaceChild(ref, c['domRef']);
    });
  }
  // step 3: call destroyed hooks, call created hooks
  callDestroyedRecursive(c);
}

// destroy vtext, vnode, vcomp
function destroy(obj: VTree, parent: Element): void {
  // step 1: invoke destroy pre-hooks on vnode and vcomp
  callBeforeDestroyedRecursive(obj);
  // step 2: destroy
  parent.removeChild(obj['domRef']);
  // step 3: invoke post-hooks for vnode and vcomp
  callDestroyedRecursive(obj);
}

function diffNodes(c: VTree, n: VTree, parent: Element): void {
  // bail out on easy vtext case
  if (c['type'] === 'vtext') {
    if (c['text'] !== n['text']) c['domRef'].textContent = n['text'];
    n['domRef'] = c['domRef'];
    return;
  }
  // check children
  if (
    c['tag'] === n['tag'] &&
    n['key'] === c['key'] &&
    n['data-component-id'] === c['data-component-id']
  ) {
    n['domRef'] = c['domRef'];
    // dmj: we will diff properties on 'vcomp' as well
    populate(c, n);
  } else {
    // dmj: we replace when things just don't line up during the diff
    replace(c, n, parent);
  }
}
// ** recursive calls to hooks
function callDestroyedRecursive(obj: VTree): void {
  callDestroyed(obj);
  for (const i in obj['children']) {
    callDestroyedRecursive(obj['children'][i]);
  }
}

function callDestroyed(obj: VTree): void {
  if (obj['onDestroyed']) obj['onDestroyed']();
}

function callBeforeDestroyed(obj: VTree): void {
  if (obj['onBeforeDestroyed']) obj['onBeforeDestroyed']();
  if (obj['type'] === 'vcomp') obj['unmount'](obj['domRef']);
}

function callBeforeDestroyedRecursive(obj: VTree): void {
  callBeforeDestroyed(obj);
  for (const i in obj['children']) {
    callBeforeDestroyedRecursive(obj['children'][i]);
  }
}

// ** </> recursive calls to hooks
export function callCreated(obj: VTree): void {
  if (obj['type'] === 'vcomp') mountComponent(obj);
  if (obj['onCreated']) obj['onCreated']();
}

export function callBeforeCreated(obj: VTree): void {
  if (obj['onBeforeCreated']) obj['onBeforeCreated']();
}

export function populate(c: VTree, n: VTree): void {
  if (n['type'] !== 'vtext') {
    if (!c) c = vnode({});
    diffProps(c['props'], n['props'], n['domRef'], n['ns'] === 'svg');
    diffCss(c['css'], n['css'], n['domRef']);
    if (n['type'] === 'vnode') {
      diffChildren(c['children'], n['children'], n['domRef']);
    }
  }
}

function diffProps(cProps: Props, nProps: Props, node: Element, isSvg: boolean): void {
  var newProp;
  /* Is current prop in new prop list? */
  for (const c in cProps) {
    newProp = nProps[c];
    /* If current property no longer exists, remove it */
    if (newProp === undefined) {
      /* current key is not in node, remove it from DOM, if SVG, remove attribute */
      if (isSvg || !(c in node)) {
        node.removeAttribute(cProps[c]);
      } else {
        node[c] = '';
      }
    } else {
      /* Already on DOM from previous diff, continue */
      if (newProp === cProps[c] && c !== 'checked' && c !== 'value') continue;
      if (isSvg) {
        if (c === 'href') {
          node.setAttributeNS('http://www.w3.org/1999/xlink', 'href', newProp);
        } else {
          node.setAttribute(c, newProp);
        }
      } else if (c in node && !(c === 'list' || c === 'form')) {
        node[c] = newProp;
      } else {
        node.setAttribute(c, newProp);
      }
    }
  }
  /* add remaining */
  for (const n in nProps) {
    if (cProps && cProps[n]) continue;
    newProp = nProps[n];
    /* Only add new properties, skip (continue) if they already exist in current property map */
    if (isSvg) {
      if (n === 'href') {
        node.setAttributeNS('http://www.w3.org/1999/xlink', 'href', newProp);
      } else {
        node.setAttribute(n, newProp);
      }
    } else if (n in node && !(n === 'list' || n === 'form')) {
      node[n] = nProps[n];
    } else {
      node.setAttribute(n, newProp);
    }
  }
}

function diffCss(cCss: CSS, nCss: CSS, node: DOMRef): void {
  var result: string;
  /* is current attribute in new attribute list? */
  for (const c in cCss) {
    result = nCss[c];
    if (!result) {
      /* current key is not in node */
      node.style[c] = '';
    } else if (result !== cCss[c]) {
      node.style[c] = result;
    }
  }
  /* add remaining */
  for (const n in nCss) {
    if (cCss && cCss[n]) continue;
    node.style[n] = nCss[n];
  }
}

function hasKeys(ns: Array<VTree>, cs: Array<VTree>): boolean {
  return ns.length > 0 && cs.length > 0 && ns[0]['key'] != null && cs[0]['key'] != null;
}

function diffChildren(cs: Array<VTree>, ns: Array<VTree>, parent: Element): void {
  const longest: number = ns.length > cs.length ? ns.length : cs.length;
  if (hasKeys(ns, cs)) {
    syncChildren(cs, ns, parent);
  } else {
    for (var i = 0; i < longest; i++) {
      diff(cs[i], ns[i], parent);
    }
  }
}

function populateDomRef(obj: VTree): void {
  if (obj['ns'] === 'svg') {
    obj['domRef'] = document.createElementNS('http://www.w3.org/2000/svg', obj['tag']);
  } else if (obj['ns'] === 'mathml') {
    obj['domRef'] = document.createElementNS('http://www.w3.org/1998/Math/MathML', obj['tag']);
  } else {
    obj['domRef'] = document.createElement(obj['tag']);
  }
}
// dmj: refactor this, the callback function feels meh
function createElement(obj: VTree, cb: (e: Node) => void): void {
  callBeforeCreated(obj);
  populateDomRef(obj);
  cb(obj['domRef']);
  populate(null, obj);
  callCreated(obj);
}
// mounts vcomp by calling into Haskell side.
// unmount is handled with pre-destroy recursive hooks
function mountComponent(obj: VTree): void {
  const componentId = obj['data-component-id'],
    nodeList = document.querySelectorAll("[data-component-id='" + componentId + "']");
  // dmj: bail out if duplicate mounting detected
  if (nodeList.length > 0) {
    console.error('AlreadyMountedException: Component "' + componentId + "' is already mounted");
    return;
  }
  // dmj, the component placeholder div[id=name] is already on the dom and vdom
  // Now we gen the component and append it to the vdom and real dom
  (obj['domRef'] as Element).setAttribute('data-component-id', componentId);
  // ^ we have to set this before 'mount()' is called, since `diff` requires it.
  obj['mount']((component: VComp) => {
    // mount() gives us the VTree from the Haskell side, so we just attach it here
    // to tie the knot (attach to both vdom and real dom).
    obj['children'].push(component);
    obj['domRef'].appendChild(component['domRef']);
  });
}
// creates nodes on virtual and dom (vtext, vcomp, vnode)
function create(obj: VTree, parent: Element): void {
  if (obj['type'] === 'vtext') {
    obj['domRef'] = document.createTextNode(obj['text']);
    parent.appendChild(obj['domRef']);
  } else {
    createElement(obj, (e: Element) => {
      parent.appendChild(e);
    });
  }
}
/* Child reconciliation algorithm, inspired by kivi and Bobril */
function syncChildren(os: Array<VTree>, ns: Array<VTree>, parent: Element): void {
  var oldFirstIndex: number = 0,
    newFirstIndex: number = 0,
    oldLastIndex: number = os.length - 1,
    newLastIndex: number = ns.length - 1,
    tmp: number,
    nFirst: VTree,
    nLast: VTree,
    oLast: VTree,
    oFirst: VTree,
    found: boolean,
    node: VTree;
  for (;;) {
    /* check base case, first > last for both new and old
              [ ] -- old children empty (fully-swapped)
              [ ] -- new children empty (fully-swapped)
              */
    if (newFirstIndex > newLastIndex && oldFirstIndex > oldLastIndex) {
      break;
    }
    /* Initialize */
    nFirst = ns[newFirstIndex];
    nLast = ns[newLastIndex];
    oFirst = os[oldFirstIndex];
    oLast = os[oldLastIndex];
    /* No more old nodes, create and insert all remaining nodes
               -> [ ] <- old children
               -> [ a b c ] <- new children
               */
    if (oldFirstIndex > oldLastIndex) {
      diff(null, nFirst, parent);
      /* insertBefore's semantics will append a node if the second argument provided is `null` or `undefined`.
         Otherwise, it will insert node['domRef'] before oLast['domRef']. 
      */
      parent.insertBefore(nFirst['domRef'], oFirst ? oFirst['domRef'] : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    } /* No more new nodes, delete all remaining nodes in old list
         -> [ a b c ] <- old children
         -> [ ] <- new children 
      */ 
    else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        parent.removeChild(os[oldLastIndex--]['domRef']);
      }
      os.splice(oldFirstIndex, tmp - oldFirstIndex + 1);
      break;
    } 
    
    else if (oFirst['key'] === nFirst['key']) {
      /* happy path, everything aligns, we continue
         -> oldFirstIndex -> [ a b c ] <- oldLastIndex
         -> newFirstIndex -> [ a b c ] <- newLastIndex
         check if nFirst and oFirst align, if so, check nLast and oLast
      */
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent);
    } 
    else if (oLast['key'] === nLast['key']) {
      diff(os[oldLastIndex--], ns[newLastIndex--], parent);
    } /* flip-flop case, nodes have been swapped, in some way or another
               both could have been swapped.
               -> [ a b c ] <- old children
               -> [ c b a ] <- new children
      */ 
     else if (oFirst['key'] === nLast['key'] && nFirst['key'] === oLast['key']) {
      swapDOMRefs(oLast['domRef'], oFirst['domRef'], parent);
      swap<VTree>(os, oldFirstIndex, oldLastIndex);
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent);
    } /* Or just one could be swapped (d's align here)
        This is top left and bottom right match case.
        We move d to end of list, mutate old vdom to reflect the change
        We then continue without affecting indexes, hoping to land in a better case
        -> [ d a b ] <- old children
        -> [ a b d ] <- new children
        becomes
        -> [ a b d ] <- old children
        -> [ a b d ] <- new children
        and now we happy path */ 
    else if (oFirst['key'] === nLast['key']) {
      /* insertAfter */
      parent.insertBefore(oFirst['domRef'], oLast['domRef'].nextSibling);
      /* swap positions in old vdom */
      os.splice(oldLastIndex, 0, os.splice(oldFirstIndex, 1)[0]);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent);
    } /* This is top right and bottom lefts match case.
         We move d to end of list, mutate old vdom to reflect the change
         -> [ b a d ] <- old children
         -> [ d b a ] <- new children
         becomes
         -> [ d b a ] <- old children
         -> [ d b a ] <- new children
         and now we happy path */ 
    else if (oLast['key'] === nFirst['key']) {
      /* insertAfter */
      parent.insertBefore(oLast['domRef'], oFirst['domRef']);
      /* swap positions in old vdom */
      os.splice(oldFirstIndex, 0, os.splice(oldLastIndex, 1)[0]);
      diff(os[oldFirstIndex++], nFirst, parent);
      newFirstIndex++;
    } /* The 'you're screwed' case, nothing aligns, pull the ripcord,
        do something more fancy. This can happen when the list is sorted, for example.
        -> [ a e c ] <- old children
        -> [ b e d ] <- new children
     */ 
    else {
      /* final case, perform linear search to check
          if new key exists in old map, decide what to do from there
       */
      found = false;
      tmp = oldFirstIndex;
      while (tmp <= oldLastIndex) {
        if (os[tmp]['key'] === nFirst['key']) {
          found = true;
          node = os[tmp];
          break;
        }
        tmp++;
      }
      /* If new key was found in old map this means it was moved, hypothetically as below
         -> [ a e b c ] <- old children
         -> [ b e a j ] <- new children
              ^
         In the above case 'b' has been moved, so we need to insert 'b' before
         'a' in both vDOM and DOM. We also increase oldFirstIndex and newFirstIndex.
         This results in new list below w/ updated index position
          -> [ b a e c ] <- old children
          -> [ b e a j ] <- new children
                   ^
      */
      if (found) {
        /* Move item to correct position */
        os.splice(oldFirstIndex, 0, os.splice(tmp, 1)[0]);
        /* optionally perform `diff` here */
        diff(os[oldFirstIndex++], nFirst, parent);
        /* Swap DOM references */
        parent.insertBefore(node['domRef'], os[oldFirstIndex]['domRef']);
        /* increment counters */
        newFirstIndex++;
      } /* If new key was *not* found in the old map this means it must now be created, example below
        -> [ a e d c ] <- old children
        -> [ b e a j ] <- new children
             ^
        In the above case 'b' does not exist in the old map, so we create a new element and DOM reference.
        We then insertBefore in both vDOM and DOM.

        -> [ b a e d c ] <- old children
        -> [ b e a j   ] <- new children
             ^
        */ else {
        createElement(nFirst, (e: Element) => {
          parent.insertBefore(e, oFirst['domRef']);
        });
        os.splice(oldFirstIndex++, 0, nFirst);
        newFirstIndex++;
        oldLastIndex++;
      }
    }
  }
}

function swapDOMRefs(a: Node, b: Node, p: Node): void {
  const tmp = a.nextSibling;
  p.insertBefore(a, b);
  p.insertBefore(b, tmp);
}

function swap<T>(os: Array<T>, l: number, r: number): void {
  const k = os[l];
  os[l] = os[r];
  os[r] = k;
}
