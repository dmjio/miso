import { Context, VNode, VComp, ComponentId, DOMRef, VTree, Props, CSS } from './types';
import { vnode } from './smart';

/* virtual-dom diffing algorithm, applies patches as detected */
export function diff(currentObj: VTree, newObj: VTree, parent: Element, context: Context): void {
  if (!currentObj && !newObj) return;
  else if (!currentObj && newObj) create(newObj, parent, context);
  else if (!newObj) destroy(currentObj, parent, context);
  else {
    if (currentObj['type'] === newObj['type']) {
        diffNodes(currentObj, newObj, parent, context);
    } else {
        replace(currentObj, newObj, parent, context);
    }
  }
}
// replace everything function
function replace(c: VTree, n: VTree, parent: Element, context : Context): void {
  // step1 : prepare to delete, unmount things
  callBeforeDestroyedRecursive(c);
  // ^ this will unmount sub components before we replace the child
  // and will recursively call hooks on nodes
  // step2 : create new things, replace old things with new things
  if (n['type'] === 'vtext') {
    n['domRef'] = context['createTextNode'](n['text']);
    context['replaceChild'](parent, n['domRef'], c['domRef']);
  } else {
    createElement(n, context, (newChild: Element) => {
      context['replaceChild'](parent, newChild, c['domRef']);
    });
  }
  // step 3: call destroyed hooks, call created hooks
  callDestroyedRecursive(c);
}

// destroy vtext, vnode, vcomp
function destroy(obj: VTree, parent: Element, context: Context): void {
  // step 1: invoke destroy pre-hooks on vnode and vcomp
  callBeforeDestroyedRecursive(obj);
  // step 2: destroy
  context['removeChild'](parent, obj['domRef']);
  // step 3: invoke post-hooks for vnode and vcomp
  callDestroyedRecursive(obj);
}

function diffNodes(c: VTree, n: VTree, parent: Element, context: Context): void {
  // bail out on easy vtext case
  if (c['type'] === 'vtext') {
    if (c['text'] !== n['text']) {
      context['setTextContent'](c['domRef'], n['text']);
    }
    n['domRef'] = c['domRef'];
    return;
  }
  if (
    n['tag'] === c['tag'] &&
    n['key'] === c['key'] &&
    n['type'] === c['type']
  ) {
      if (n['type'] !== 'vcomp') {

          /* dmj:

             If both 'n' and 'c' are components with identical keys, don't call `mount()` on `n`.
             Just assign it 'c'.

             This is the edge case where users might be confused as to why their Component
             aren't being replaced. This requires a key_ identifier (i.e. `StableName` in Haskell
             nomenclature) placed by *the user* in order to differentiate if a Component
             should be replaced or not.

             `key_` is overloaded to operate on child lists (`syncChildren` operations)
             and also as a stable name identifier in Component diffing.

           */

        n['domRef'] = c['domRef'];
        populate(c, n, context);

      }

  } else {
    // dmj: we replace when things just don't line up during the diff
    replace(c, n, parent, context);
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
  if (obj['type'] === 'vcomp') unmountComponent(obj);
}

function callBeforeDestroyed(obj: VTree): void {
  if (obj['onBeforeDestroyed']) obj['onBeforeDestroyed']();
}

function callBeforeDestroyedRecursive(obj: VTree): void {
  if (obj['type'] === 'vcomp' && obj['onBeforeUnmounted']) {
    obj['onBeforeUnmounted']();
  }
  callBeforeDestroyed(obj);
  for (const i in obj['children']) {
    callBeforeDestroyedRecursive(obj['children'][i]);
  }
}

// ** </> recursive calls to hooks
export function callCreated(obj: VTree, context: Context): void {
  if (obj['onCreated']) obj['onCreated'](obj['domRef']);
  if (obj['type'] === 'vcomp') mountComponent(obj);
}

export function callBeforeCreated(obj: VTree): void {
  if (obj['onBeforeCreated']) obj['onBeforeCreated']();
}

export function populate(c: VTree, n: VTree, context: Context): void {
  if (n['type'] !== 'vtext') {
    if (!c) c = vnode({});
    diffProps(c['props'], n['props'], n['domRef'], n['ns'] === 'svg', context);
    diffCss(c['css'], n['css'], n['domRef'], context);
    if (n['type'] === 'vnode') {
      diffChildren(c as VNode, n as VNode, n['domRef'], context);
    }
    drawCanvas(n);
  }
}

function diffProps(cProps: Props, nProps: Props, node: Element, isSvg: boolean, context: Context): void {
  var newProp;
  /* Is current prop in new prop list? */
  for (const c in cProps) {
    newProp = nProps[c];
    /* If current property no longer exists, remove it */
    if (newProp === undefined) {
      /* current key is not in node, remove it from DOM, if SVG, remove attribute */
      if (isSvg || !(c in node)) {
        context['removeAttribute'](node, c);
      } else {
        context['setAttribute'](node, c, '');
      }
    } else {
      /* Already on DOM from previous diff, continue */
      if (newProp === cProps[c] && c !== 'checked' && c !== 'value') continue;
      if (isSvg) {
        if (c === 'href') {
          context['setAttributeNS'](node, 'http://www.w3.org/1999/xlink', 'href', newProp);
        } else {
          context['setAttribute'](node, c, newProp);
        }
      } else if (c in node && !(c === 'list' || c === 'form')) {
          node[c] = newProp;
      } else {
          context['setAttribute'](node, c, newProp);
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
        context['setAttributeNS'](node, 'http://www.w3.org/1999/xlink', 'href', newProp);
      } else {
        context['setAttribute'](node, n, newProp);
      }
    } else if (n in node && !(n === 'list' || n === 'form')) {
      node[n] = nProps[n];
    } else {
      context['setAttribute'](node, n, newProp);
    }
  }
}

function diffCss(cCss: CSS, nCss: CSS, node: DOMRef, context: Context): void {
  context['setInlineStyle'](cCss, nCss, node);
}

function diffChildren(c: VNode, n: VNode, parent: Element, context: Context): void {
  if (c['shouldSync'] && n['shouldSync']) {
    syncChildren(c.children, n.children, parent, context);
  } else {
    const longest: number =
      n.children.length > c.children.length
        ? n.children.length
        : c.children.length;

    for (let i = 0; i < longest; i++)
      diff(c.children[i], n.children[i], parent, context);
  }
}

function populateDomRef(obj: VTree, context: Context): void {
  if (obj['ns'] === 'svg') {
    (obj['domRef'] as Element) = context['createElementNS']('http://www.w3.org/2000/svg', obj['tag']);
  } else if (obj['ns'] === 'mathml') {
    (obj['domRef'] as Element)= context['createElementNS']('http://www.w3.org/1998/Math/MathML', obj['tag']);
  } else {
    /* calling createElement on doucment */
    (obj['domRef'] as Element) = context['createElement'](obj['tag']);
  }
}
// dmj: refactor this, the callback function feels meh
function createElement(obj: VTree, context: Context, attach: (e: Node) => void): void {
  callBeforeCreated(obj);
  populateDomRef(obj, context);
  callCreated(obj, context);
  attach(obj['domRef']);
  populate(null, obj, context);
}

/* draw the canvas if you need to */
function drawCanvas (obj: VTree) {
  if (obj['tag'] === 'canvas' && 'draw' in obj) {
    obj['draw'](obj['domRef']);
  }
}

// unmount components
function unmountComponent(obj: VTree): void {
  if ('onUnmounted' in obj) obj['onUnmounted'](obj['domRef']);
  if ('unmount' in obj) obj['unmount']();
}

// mounts vcomp by calling into Haskell side.
// unmount is handled with pre-destroy recursive hooks
function mountComponent(obj: VComp): void {
  if (obj['onBeforeMounted']) obj['onBeforeMounted']();
  obj['mount'](obj);
  if (obj['onMounted']) obj['onMounted'](obj['domRef']);
}
// creates nodes on virtual dom (vtext, vcomp, vnode)
function create(obj: VTree, parent: Element, context: Context): void {
  if (obj['type'] === 'vtext') {
    obj['domRef'] = context['createTextNode'](obj['text']);
    context['appendChild'](parent, obj['domRef']);
  } else {
    createElement(obj, context, (child: Element) => {
      context['appendChild'](parent, child);
    });
  }
}
/* Child reconciliation algorithm, inspired by kivi and Bobril */
function syncChildren(os: Array<VTree>, ns: Array<VTree>, parent: Element, context: Context): void {
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
      diff(null, nFirst, parent, context);
      /* insertBefore's semantics will append a node if the second argument provided is `null` or `undefined`.
         Otherwise, it will insert node['domRef'] before oLast['domRef'].
      */
      context['insertBefore'](parent, nFirst['domRef'], oFirst ? oFirst['domRef'] : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    } /* No more new nodes, delete all remaining nodes in old list
         -> [ a b c ] <- old children
         -> [ ] <- new children
      */
    else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        context['removeChild'](parent, os[oldLastIndex--]['domRef']);
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
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent, context);
    }
    else if (oLast['key'] === nLast['key']) {
        diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
    } /* flip-flop case, nodes have been swapped, in some way or another
               both could have been swapped.
               -> [ a b c ] <- old children
               -> [ c b a ] <- new children
      */
     else if (oFirst['key'] === nLast['key'] && nFirst['key'] === oLast['key']) {
      context['swapDOMRefs'](oLast['domRef'], oFirst['domRef'], parent);
      swap<VTree>(os, oldFirstIndex, oldLastIndex);
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent, context);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
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
      context['insertBefore'](parent, oFirst['domRef'], oLast['domRef'].nextSibling);
      /* swap positions in old vdom */
      os.splice(oldLastIndex, 0, os.splice(oldFirstIndex, 1)[0]);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
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
      context['insertBefore'](parent, oLast['domRef'], oFirst['domRef']);
      /* swap positions in old vdom */
      os.splice(oldFirstIndex, 0, os.splice(oldLastIndex, 1)[0]);
      diff(os[oldFirstIndex++], nFirst, parent, context);
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
        diff(os[oldFirstIndex++], nFirst, parent, context);
        /* Swap DOM references */
        context['insertBefore'](parent, node['domRef'], os[oldFirstIndex]['domRef']);
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
        createElement(nFirst, context, (e: Element) => {
          context['insertBefore'](parent, e, oFirst['domRef']);
        });
        os.splice(oldFirstIndex++, 0, nFirst);
        newFirstIndex++;
        oldLastIndex++;
      }
    }
  }
}

function swap<T>(os: Array<T>, l: number, r: number): void {
  const k = os[l];
  os[l] = os[r];
  os[r] = k;
}
