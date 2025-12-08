import { Class, DrawingContext, VNode
       , VComp, ComponentId, VTree, Props, CSS
       , VText
       }
from './types';
import { vnode } from './smart';
import { drill } from './util';

/* virtual-dom diffing algorithm, applies patches as detected */
export function diff<T>(c: VTree<T>, n: VTree<T>, parent: T, context: DrawingContext<T>): void {
    if (!c && !n)
        return;
    else if (!c)
        create(n, parent, context);
    else if (!n)
        destroy(c, parent, context);
    else if (c.type === 'vtext' && n.type === 'vtext')
        diffVText(c, n, parent, context);
    else if (c.type === 'vnode' && n.type === 'vnode')
        diffVNode(c, n, parent, context);
    else if (c.type === 'vcomp' && n.type === 'vcomp')
        diffVComp(c, n, parent, context);
    else
        replace(c, n, parent, context);
}

function diffVComp<T>(c: VComp<T>, n: VComp<T>, parent: T, context: DrawingContext<T>) : void {
    if (c.key === n.key) {
      n.child = c.child;
      return;
    }
    replace (c,n,parent,context);
}

function diffVText<T>(c: VText<T>, n: VText<T>, parent: T, context: DrawingContext<T>) : void {
  if (c.text !== n.text) {
    context.setTextContent(c.domRef, n.text);
  }
  n.domRef = c.domRef;
  return;
}

function diffVNode<T>(c: VNode<T>, n: VNode<T>, parent: T, context: DrawingContext<T>): void {
  if (n.tag === c.tag && n.key === c.key) {
    n.domRef = c.domRef;
    populate(c, n, context);
  } else {
    replace(c, n, parent, context);
  }
}

// replace everything function
function replace<T>(c: VTree<T>, n: VTree<T>, parent: T, context : DrawingContext<T>): void {
  // step1 : prepare to delete, unmount things
  callBeforeDestroyedRecursive(c);
  // ^ this will unmount sub components before we replace the child
  // and will recursively call hooks on nodes
  // step2 : create new things, replace old things with new things
  switch (c.type) {
    case 'vtext':
      switch (n.type) {
        case 'vtext':
          /* replace vtext w/ vtext */
          n.domRef = context.createTextNode(n.text);
          context.replaceChild(parent, n.domRef, c.domRef);
          break;
        case 'vnode':
          /* replace vtext w/ vnode */
          context.replaceChild(parent, createVNode(n, context), c.domRef);
          break;
        case 'vcomp':
          /* replace vtext w/ vcomp */
          mountComponent(parent, n, c.domRef, context, false);
          break;
      } break;
    case 'vnode':
      switch (n.type) {
        case 'vtext':
          /* replace vnode w/ vtext */
          n.domRef = context.createTextNode(n.text);
          context.replaceChild(parent, n.domRef, c.domRef);
          break;
        case 'vnode':
          /* replace vnode w/ vnode */
          context.replaceChild(parent, createVNode(n, context), c.domRef as T);
          break;
        case 'vcomp':
          /* replace vnode w/ vcomp */
          mountComponent(parent, n, c.domRef, context, false);
          break;
      } break;
      case 'vcomp':
        switch (n.type) {
          case 'vtext':
             /* replace vcomp w/ vtext */
             n.domRef = context.createTextNode(n.text);
             context.replaceChild(parent, n.domRef, drill(c) as T);
            break;
          case 'vnode':
            /* replace vcomp w/ vnode */
            context.replaceChild(parent, createVNode(n, context), drill(c) as T);
            break;
          case 'vcomp':
            /* replace vcomp w/ vcomp */
            mountComponent (parent, n, drill(c), context, false);
            break;
        } break;
  }

  // step 3: call destroyed hooks, call created hooks
  callDestroyedRecursive(c);
}

// destroy vtext, vnode, vcomp
function destroy<T>(obj: VTree<T>, parent: T, context: DrawingContext<T>): void {
  // step 1: invoke destroy pre-hooks on vnode and vcomp
  callBeforeDestroyedRecursive(obj);
  // step 2: destroy
  switch (obj.type) {
     case 'vcomp':
        destroy(obj.child, parent, context);
        break;
      default:
        context.removeChild(parent, obj.domRef);
        break;
  }
  // step 3: invoke post-hooks for vnode and vcomp
  callDestroyedRecursive(obj);
}

// ** recursive calls to hooks
function callDestroyedRecursive<T>(obj: VTree<T>): void {
  switch (obj.type) {
      case 'vtext':
          break;
      case 'vnode':
           callDestroyed(obj);
           for (const i in obj.children) {
             callDestroyedRecursive(obj.children[i]);
           }
          break;
      case 'vcomp':
          unmountComponent(obj);
          callDestroyedRecursive(obj.child);
          break;
  }
}

function callDestroyed<T>(obj: VTree<T>): void {
  switch (obj.type) {
      case 'vcomp':
          unmountComponent(obj);
          break;
      case 'vnode':
          if (obj['onDestroyed']) obj.onDestroyed();
          break;
      case 'vtext':
          break;
  }
}

function callBeforeDestroyedRecursive<T>(obj: VTree<T>): void {
  switch (obj.type) {
    case 'vcomp':
      if (obj['onBeforeUnmounted']) obj.onBeforeUnmounted();
      callBeforeDestroyedRecursive(obj.child);
      break;
    case 'vnode':
      if (obj['onBeforeDestroyed']) obj.onBeforeDestroyed();
      for (const i in obj.children)
        callBeforeDestroyedRecursive(obj.children[i]);
      break;
    case 'vtext':
      break;
  }
}

// ** </> recursive calls to hooks
export function callCreated<T>(obj: VNode<T>, context: DrawingContext<T>): void {
  if (obj.onCreated) obj.onCreated();
}

export function callBeforeCreated<T>(vnode: VNode<T>): void {
  if (vnode.onBeforeCreated) vnode.onBeforeCreated();
}

export function populate<T>(c: VNode<T>, n: VNode<T>, context: DrawingContext<T>): void {
  if (!c) c = vnode({}); // dmj: get rid of this
  diffProps(c.props, n.props, n.domRef, n.ns === 'svg', context);
  diffClass(c.classList, n.classList, n.domRef, context);
  diffCss(c.css, n.css, n.domRef, context);
  diffChildren(c, n, n.domRef, context);
  drawCanvas(n);
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
        context['setAttributeNS'](node, 'http://www.w3.org/1999/xlink', 'href', newProp);
      } else {
        context['setAttribute'](node, n, newProp);
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

function shouldSync<T> (c: VNode<T>, n: VNode<T>) {
    if (c.children.length === 0 || n.children.length === 0) return false;
    for (var i = 0; i < c.children.length; i++) {
        if (c.children[i].key === null || c.children[i].key === undefined) {
            return false;
        }
    }
    for (var i = 0; i < n.children.length; i++) {
        if (n.children[i].key === null || n.children[i].key === undefined) {
            return false;
        }
    }
    return true;
}

function diffChildren<T>(c: VNode<T>, n: VNode<T>, parent: T, context: DrawingContext<T>): void {
  if (shouldSync(c,n)) {
    syncChildren(c.children, n.children, parent, context);
  } else {
    for (let i = 0; i < Math.max (n.children.length, c.children.length); i++)
      diff(c.children[i], n.children[i], parent, context);
  }
}

function populateDomRef<T>(obj: VNode<T>, context: DrawingContext<T>): void {
  switch (obj.ns) {
    case 'svg':
      obj.domRef = context.createElementNS('http://www.w3.org/2000/svg', obj.tag);
      break;
    case 'mathml':
      obj.domRef = context.createElementNS('http://www.w3.org/1998/Math/MathML', obj.tag);
      break;
    case 'html':
      obj.domRef = context.createElement(obj.tag);
      break;
    }
}

function createVNode<T>(vnode: VNode<T>, context: DrawingContext<T>): T {
  callBeforeCreated(vnode);
  populateDomRef(vnode, context);
  callCreated(vnode, context);
  populate(null, vnode, context);
  return vnode.domRef;
}

/* draw the canvas if you need to */
function drawCanvas<T> (obj: VNode<T>) {
  if (obj.tag === 'canvas' && 'draw' in obj) {
    obj.draw(obj.domRef);
  }
}

// unmount components
function unmountComponent<T>(obj: VComp<T>): void {
  if ('onUnmounted' in obj) obj.onUnmounted();
  obj.unmount(obj.componentId);
}

// mounts vcomp by calling into Haskell side.
// unmount is handled with pre-destroy recursive hooks
export function mountComponent<T>(parent: T, vcomp: VComp<T>, c: T, context: DrawingContext<T>, insertBefore: boolean): void {
  if (vcomp.onBeforeMounted) vcomp.onBeforeMounted();

  // Call 'onBeforeMounted' before calling 'mount'
  vcomp.mount(parent, (componentId: ComponentId, n: VTree<T>) => {
    // mount() gives us the VTree from the Haskell side, so we just attach it here
    // to tie the knot (attach to both vdom and real dom).

    // dmj: populated children means mounted, invariant for delegation.
    // hence why mounting must be synchronous
    vcomp.componentId = componentId;
    vcomp.child = n;
    if (vcomp.onMounted) vcomp.onMounted();

    switch (n.type) {
      case 'vcomp':
        /* recursive mount case */
        mountComponent (parent, n, c, context, false);
        break;
      default:
         if (c) {
             if (insertBefore) {
               context.insertBefore(parent, n.domRef, c);
             } else {
               context.replaceChild(parent, n.domRef, c);
             }
         }
        else context.appendChild(parent, n.domRef);
        break;
    }

  });
}

// creates nodes on vdom and dom (vtext, vcomp, vnode)
function create<T>(vtree: VTree<T>, parent: T, context: DrawingContext<T>): void {
   switch (vtree.type) {
     case 'vtext':
       vtree.domRef = context.createTextNode(vtree.text);
       context.appendChild(parent, vtree.domRef);
       break;
     case 'vnode':
       context.appendChild(parent, createVNode(vtree, context));
       break;
     case 'vcomp':
       mountComponent (parent, vtree, null, context, false);
       break;
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
      context.insertBefore(parent, drill(nFirst), oFirst ? drill(oFirst) : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    } /* No more new nodes, delete all remaining nodes in old list
         -> [ a b c ] <- old children
         -> [ ] <- new children
      */
    else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        context.removeChild(parent, drill(os[oldLastIndex--]));
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
      context.swapDOMRefs(drill(oLast), drill(oFirst), parent);
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
      context.insertBefore(parent, drill(oFirst), context.nextSibling(oLast));
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
      context.insertBefore(parent, drill(oLast), drill(oFirst));
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
        context.insertBefore(parent, drill(node), drill(os[oldFirstIndex]));
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
                case 'vtext':
                    nFirst.domRef = context.createTextNode(nFirst.text);                    
                    context.insertBefore(parent, nFirst.domRef, drill(oFirst));
                    break;
                case 'vcomp':
                    mountComponent (parent, nFirst, drill(oFirst), context, true);
                    break;
                case 'vnode':
                    context.insertBefore(parent, createVNode(nFirst, context), drill(oFirst));
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
