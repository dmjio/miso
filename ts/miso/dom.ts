import { Class, DrawingContext, CSS, VNode, VText, VComp, ComponentId, VTree, Props, VTreeType } from './types';

/* virtual-dom diffing algorithm, applies patches as detected */
export function diff<T>(c: VTree<T>, n: VTree<T>, parent: T, context: DrawingContext<T>): void {
    if (!c && !n)
        return;
    else if (!c)
        create(n, parent, context);
    else if (!n)
        destroy(c, parent, context);
    else if (c.type === VTreeType.VText && n.type === VTreeType.VText) {
        diffVText(c, n, context);
    }
    else if (c.type === VTreeType.VComp && n.type === VTreeType.VComp) {
        if (n.tag === c.tag && n.key === c.key) {
          n.domRef = c.domRef;
          diffAttrs(c, n, context);
        } else {
          replace(c, n, parent, context);
        }
    }
    else if (c.type === VTreeType.VNode && n.type === VTreeType.VNode) {
        if (n.tag === c.tag && n.key === c.key) {
          n.domRef = c.domRef;
          diffAttrs(c, n, context);
        } else {
          replace(c, n, parent, context);
        }
    }
    else
        replace(c, n, parent, context);
}

function diffVText<T>(c: VText<T>, n: VText<T>, context : DrawingContext<T>): void {
  if (c.text !== n.text) context.setTextContent(c.domRef, n.text);
  n.domRef = c.domRef;
  return;
}


// replace everything function
function replace<T>(c: VTree<T>, n: VTree<T>, parent: T, context : DrawingContext<T>): void {
  // step1 : prepare to delete, unmount things
  switch (c.type) {
      case VTreeType.VText:
          break;
      default:
          callBeforeDestroyedRecursive(c);
          break;
  }
  switch (n.type) {
      case VTreeType.VText:
          switch (c.type) {
              default:
                n.domRef = context.createTextNode(n.text);
                context.replaceChild(parent, n.domRef, c.domRef);
                break;
          }
          break;
      default:
        context.replaceChild(parent, createElement(n, context), c.domRef as T);
        break;
  }
  // step 3: call destroyed hooks, call created hooks
  switch (c.type) {
      case VTreeType.VText:
          break;
      default:
          callDestroyedRecursive(c);
          break;
  }
}

// destroy vtext, vnode, vcomp
function destroy<T>(c: VTree<T>, parent: T, context: DrawingContext<T>): void {
  // step 1: invoke destroy pre-hooks on vnode and vcomp
  switch (c.type) {
      case VTreeType.VText:
          break;
      default:
          callBeforeDestroyedRecursive(c);
          break;
  }
  // step 2: destroy
  context.removeChild(parent, c.domRef);
  // step 3: invoke post-hooks for vnode and vcomp
  switch (c.type) {
      case VTreeType.VText:
          break;
      default:
          callDestroyedRecursive(c);
          break;
  }
}

// ** recursive calls to hooks
function callDestroyedRecursive<T>(c: VNode<T> | VComp<T>): void {
  callDestroyed(c);
  for (const child of c.children) {
    if (child.type === VTreeType.VNode || child.type === VTreeType.VComp) {
       callDestroyedRecursive(child);
    }
  }
}

function callDestroyed<T>(c: VNode<T> | VComp<T>): void {
  if (c.type === VTreeType.VNode && c.onDestroyed) c.onDestroyed();
  if (c.type === VTreeType.VComp) unmountComponent(c);
}

function callBeforeDestroyed<T>(c: VNode<T> | VComp<T>): void {
  switch (c.type) {
      case VTreeType.VComp:
          if (c.onBeforeUnmounted) c.onBeforeUnmounted();
          break;
      case VTreeType.VNode:
          if (c.onBeforeDestroyed) c.onBeforeDestroyed();
          break;
      default:
          break;
  }
}

function callBeforeDestroyedRecursive<T>(c: VNode<T> | VComp<T>): void {
  callBeforeDestroyed(c);
  for (const child of c.children)
    if (child.type === VTreeType.VNode || child.type === VTreeType.VComp)
       callBeforeDestroyedRecursive(child);
}

export function diffAttrs<T>(c: VNode<T> | VComp<T> | null, n: VNode<T> | VComp<T>, context: DrawingContext<T>): void {
    diffProps(c ? c.props : {}, n.props, n.domRef, n.ns === 'svg', context);
    diffClass(c ? c.classList : null, n.classList, n.domRef, context);
    diffCss(c ? c.css : {}, n.css, n.domRef, context);
    if (n.type === VTreeType.VNode) {
      diffChildren(c ? c.children : [], n.children, n.domRef, context);
      drawCanvas(n);
    }
}

export function diffClass<T> (c: Class, n: Class, domRef: T, context: DrawingContext<T>): void {
    /* base case */
    if (!c && !n) {
      return;
    }

    /* add-only case */
    if (!c) {
        for (const className of n) {
            context.addClass(className, domRef);
        }
        return;
    }

    /* remove-only case */
    if (!n) {
        for (const className of c) {
            context.removeClass(className, domRef);
        }
        return;
    }

    /* diff case */
    for (const className of c) {
        if (!(n.has(className))) {
            context.removeClass(className, domRef);
        }
    }

    for (const className of n) {
        if (!(c.has(className))) {
            context.addClass(className, domRef);
        }
    }
    return;
}

function diffProps<T extends Object>(cProps: Props, nProps: Props, node: T, isSvg: boolean, context: DrawingContext<T>): void {
  var newProp;
  /* Is current prop in new prop list? */
  for (const c in cProps) {
    newProp = nProps[c];
    /* If current property no longer exists, remove it */
    if (newProp === undefined) {
      /* current key is not in node, remove it from DOM, if SVG, remove attribute */
      if (isSvg || !(c in node) || c === 'disabled') {
        context.removeAttribute(node, c);
      } else {
        context.setAttribute(node, c, '');
      }
    } else {
      /* Already on DOM from previous diff, continue */
      if (newProp === cProps[c] && c !== 'checked' && c !== 'value') continue;

      if (isSvg) {
        if (c === 'href') {
          context.setAttributeNS(node, 'http://www.w3.org/1999/xlink', 'href', newProp);
        } else {
          context.setAttribute(node, c, newProp);
        }
      } else if (c in node && !(c === 'list' || c === 'form')) {
          node[c] = newProp;
      } else {
          context.setAttribute(node, c, newProp);
      }
    }
  }
  /* add remaining */
  for (const n in nProps) {
    /* Only add new properties, skip (continue) if they already exist in current property map */
    if (cProps && cProps[n]) continue;
    newProp = nProps[n];
    if (isSvg) {
      if (n === 'href') {
        context.setAttributeNS(node, 'http://www.w3.org/1999/xlink', 'href', newProp);
      } else {
        context.setAttribute(node, n, newProp);
      }
    } else if (n in node && !(n === 'list' || n === 'form')) {
      node[n] = nProps[n];
    } else {
      context.setAttribute(node, n, newProp);
    }
  }
}

function diffCss<T>(cCss: CSS, nCss: CSS, node: T, context: DrawingContext<T>): void {
  context.setInlineStyle(cCss, nCss, node);
}

function shouldSync<T> (cs: Array<VTree<T>>, ns: Array<VTree<T>>) {
    if (cs.length === 0 || ns.length === 0) return false;
    for (var i = 0; i < cs.length; i++) {
        if (cs[i].key === null || cs[i].key === undefined) {
            return false;
        }
    }
    for (var i = 0; i < ns.length; i++) {
        if (ns[i].key === null || ns[i].key === undefined) {
            return false;
        }
    }
    return true;
}

function diffChildren<T>(cs: Array<VTree<T>>, ns: Array<VTree<T>>, parent: T, context: DrawingContext<T>): void {
  if (shouldSync(cs,ns)) {
    syncChildren(cs, ns, parent, context);
  } else {
    for (let i = 0; i < Math.max (ns.length, cs.length); i++)
      diff(cs[i], ns[i], parent, context);
  }
}

function populateDomRef<T>(c: VComp<T> | VNode<T>, context: DrawingContext<T>): void {
  if (c.ns === 'svg') {
    c.domRef = context.createElementNS('http://www.w3.org/2000/svg', c.tag);
  } else if (c.ns === 'mathml') {
    c.domRef = context.createElementNS('http://www.w3.org/1998/Math/MathML', c.tag);
  } else {
    c.domRef = context.createElement(c.tag);
  }
}

/* used in hydrate.ts */
export function callCreated<T>(n: VComp<T> | VNode<T>, context: DrawingContext<T>): T {
  switch (n.type) {
      case VTreeType.VComp:
          mountComponent(n, context);
          break;
      case VTreeType.VNode:
          if (n.onCreated) n.onCreated(n.domRef);
          break;
  }
  return n.domRef;
}

function createElement<T>(n: VComp<T> | VNode<T>, context: DrawingContext<T>): T {
  switch (n.type) {
    case VTreeType.VComp:
      if (n.onBeforeMounted) n.onBeforeMounted();
      populateDomRef(n, context);
      mountComponent(n, context);
      break;
    case VTreeType.VNode:
      if (n.onBeforeCreated) n.onBeforeCreated();
      populateDomRef(n, context);
      if (n.onCreated) n.onCreated(n.domRef);
      break;
  }
  diffAttrs(null, n, context);
  return n.domRef;
}

/* draw the canvas if you need to */
function drawCanvas<T> (c: VNode<T>) {
  if (c.tag === 'canvas' && c.draw)
    c.draw(c.domRef);
}

// unmount components
function unmountComponent<T>(c: VComp<T>): void {
  if (c.onUnmounted) c.onUnmounted(c.domRef);
  c.unmount(c.domRef);
}

// mounts vcomp by calling into Haskell side.
// unmount is handled with pre-destroy recursive hooks
function mountComponent<T>(obj: VComp<T>, context: DrawingContext<T>): void {
  obj.mount(obj, (componentId: ComponentId, componentTree: VNode<T>) => {
    // mount() gives us the VTree from the Haskell side, so we just attach it here
    // to tie the knot (attach to both vdom and real dom).
    obj.children.push(componentTree);
    context.appendChild(obj.domRef, componentTree.domRef);
    if (obj.onMounted) obj.onMounted(obj.domRef);
  });
}
// creates nodes on virtual and dom (vtext, vcomp, vnode)
function create<T>(obj: VTree<T>, parent: T, context: DrawingContext<T>): void {
  if (obj.type === VTreeType.VText) {
    obj.domRef = context.createTextNode(obj.text);
    context.appendChild(parent, obj.domRef);
  } else {
    context.appendChild(parent, createElement(obj, context));
  }
}
/* Child reconciliation algorithm, inspired by kivi and Bobril */
function syncChildren<T>(os: Array<VTree<T>>, ns: Array<VTree<T>>, parent: T, context: DrawingContext<T>): void {
  var oldFirstIndex: number = 0,
    newFirstIndex: number = 0,
    oldLastIndex: number = os.length - 1,
    newLastIndex: number = ns.length - 1,
    tmp: number,
    nFirst: VTree<T>,
    nLast: VTree<T>,
    oLast: VTree<T>,
    oFirst: VTree<T>,
    found: boolean,
    node: VTree<T>;
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
         Otherwise, it will insert node.domRef before oLast.domRef.
      */
      context.insertBefore(parent, nFirst.domRef, oFirst ? oFirst.domRef : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    } /* No more new nodes, delete all remaining nodes in old list
         -> [ a b c ] <- old children
         -> [ ] <- new children
      */
    else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        context.removeChild(parent, os[oldLastIndex--].domRef);
      }
      os.splice(oldFirstIndex, tmp - oldFirstIndex + 1);
      break;
    }

    else if (oFirst.key === nFirst.key) {
      /* happy path, everything aligns, we continue
         -> oldFirstIndex -> [ a b c ] <- oldLastIndex
         -> newFirstIndex -> [ a b c ] <- newLastIndex
         check if nFirst and oFirst align, if so, check nLast and oLast
      */
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent, context);
    }
    else if (oLast.key === nLast.key) {
        diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
    } /* flip-flop case, nodes have been swapped, in some way or another
               both could have been swapped.
               -> [ a b c ] <- old children
               -> [ c b a ] <- new children
      */
     else if (oFirst.key === nLast.key && nFirst.key === oLast.key) {
      context.swapDOMRefs(oLast.domRef, oFirst.domRef, parent);
      swap<VTree<T>>(os, oldFirstIndex, oldLastIndex);
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
    else if (oFirst.key === nLast.key) {
      /* insertAfter */
      context.insertBefore(parent, oFirst.domRef, context.nextSibling(oLast));
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
    else if (oLast.key === nFirst.key) {
      /* insertAfter */
      context.insertBefore(parent, oLast.domRef, oFirst.domRef);
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
        if (os[tmp].key === nFirst.key) {
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
        context.insertBefore(parent, node.domRef, os[oldFirstIndex].domRef);
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
            switch (nFirst.type) {
              case VTreeType.VText:
                nFirst.domRef = context.createTextNode(nFirst.text);
                context.insertBefore(parent, nFirst.domRef, oFirst.domRef);
                break;
              default:
                context.insertBefore(parent, createElement(nFirst, context), oFirst.domRef);
                break;
            }
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
