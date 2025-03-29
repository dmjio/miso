/* virtual-dom diffing algorithm, applies patches as detected */
export function diff(currentObj: any, newObj: any, parent: any) {
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
var replace = function (c: any, n: any, parent: any) {
  // step1 : prepare to delete, unmount things
  callBeforeDestroyedRecursive(c);
  // ^ this will unmount sub components before we replace the child
  // and will recursively call hooks on nodes
  // step2 : create new things, replace old things with new things
  if (n['type'] === 'vtext') {
    n['domRef'] = document.createTextNode(n['text']);
    parent.replaceChild(n['domRef'], c['domRef']);
  } else {
    createElement(n, function (ref: any) {
      parent.replaceChild(ref, c['domRef']);
    });
  }
  // step 3: call destroyed hooks, call created hooks
  callDestroyedRecursive(c);
};

// destroy vtext, vnode, vcomp
var destroy = function (obj: any, parent: any) {
  // step 1: invoke destroy pre-hooks on vnode and vcomp
  callBeforeDestroyedRecursive(obj);
  // step 2: destroy
  parent.removeChild(obj['domRef']);
  // step 3: invoke post-hooks for vnode and vcomp
  callDestroyedRecursive(obj);
};

var diffNodes = function (c: any, n: any, parent: any) {
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
};
// ** recursive calls to hooks
var callDestroyedRecursive = function (obj: any) {
  callDestroyed(obj);
  for (var i in obj.children) {
    callDestroyedRecursive(obj.children[i]);
  }
};
var callDestroyed = function (obj: any) {
  if (obj['onDestroyed']) obj['onDestroyed']();
};
var callBeforeDestroyed = function (obj: any) {
  if (obj['onBeforeDestroyed']) obj['onBeforeDestroyed']();
  if (obj['type'] === 'vcomp') obj['unmount'](obj['domRef']);
};
var callBeforeDestroyedRecursive = function (obj: any) {
  callBeforeDestroyed(obj);
  for (var i in obj.children) {
    callBeforeDestroyedRecursive(obj.children[i]);
  }
};
// ** </> recursive calls to hooks
export function callCreated(obj: any) {
  if (obj['onCreated']) obj['onCreated']();
  if (obj['type'] === 'vcomp') mountComponent(obj);
}

export function populate(c: any, n: any) {
  if (!c) {
    c = {
      props: null,
      css: null,
      children: [],
    };
  }
  diffProps(c['props'], n['props'], n['domRef'], n['ns'] === 'svg');
  diffCss(c['css'], n['css'], n['domRef']);
  if (n['type'] === 'vcomp') return; // dmj: don't diff vcomp children
  diffChildren(c['children'], n['children'], n['domRef']);
}
var diffProps = function (cProps: any, nProps: any, node: any, isSvg: boolean) {
  var newProp;
  /* Is current prop in new prop list? */
  for (var c in cProps) {
    newProp = nProps[c];
    /* If current property no longer exists, remove it */
    if (newProp === undefined) {
      /* current key is not in node, remove it from DOM, if SVG, remove attribute */
      if (isSvg || !(c in node)) {
        node.removeAttribute(c, cProps[c]);
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
  for (var n in nProps) {
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
};
var diffCss = function (cCss: any, nCss: any, node: any) {
  var result;
  /* is current attribute in new attribute list? */
  for (var c in cCss) {
    result = nCss[c];
    if (!result) {
      /* current key is not in node */
      node.style[c] = '';
    } else if (result !== cCss[c]) {
      node.style[c] = result;
    }
  }
  /* add remaining */
  for (var n in nCss) {
    if (cCss && cCss[n]) continue;
    node.style[n] = nCss[n];
  }
};
var hasKeys = function (ns: [any], cs: [any]) {
  return (
    ns.length > 0 &&
    cs.length > 0 &&
    ns[0]['key'] != null &&
    cs[0]['key'] != null
  );
};
var diffChildren = function (cs: [any], ns: [any], parent: any) {
  var longest: number = ns.length > cs.length ? ns.length : cs.length;
  if (hasKeys(ns, cs)) {
    syncChildren(cs, ns, parent);
  } else {
    for (var i = 0; i < longest; i++) {
      diff(cs[i], ns[i], parent);
    }
  }
};
var populateDomRef = function (obj: any) {
  if (obj['ns'] === 'svg') {
    obj['domRef'] = document.createElementNS(
      'http://www.w3.org/2000/svg',
      obj['tag'],
    );
  } else if (obj['ns'] === 'mathml') {
    obj['domRef'] = document.createElementNS(
      'http://www.w3.org/1998/Math/MathML',
      obj['tag'],
    );
  } else {
    obj['domRef'] = document.createElement(obj['tag']);
  }
};
// dmj: refactor this, the callback function feels meh
var createElement = function (obj: any, cb: any) {
  populateDomRef(obj);
  cb(obj['domRef']);
  populate(null, obj);
  callCreated(obj);
};
// mounts vcomp by calling into Haskell side.
// unmount is handled with pre-destroy recursive hooks
var mountComponent = function (obj: any) {
  var componentId = obj['data-component-id'],
    nodeList = document.querySelectorAll(
      "[data-component-id='" + componentId + "']",
    );
  // dmj: bail out if duplicate mounting detected
  if (nodeList.length > 0) {
    console.error(
      'AlreadyMountedException: Component "' +
        componentId +
        "' is already mounted",
    );
    return;
  }
  // dmj, the component placeholder div[id=name] is already on the dom and vdom
  // Now we gen the component and append it to the vdom and real dom
  obj['domRef'].setAttribute('data-component-id', componentId);
  // ^ we have to set this before 'mount()' is called, since `diff` requires it.
  obj['mount'](function (component: any) {
    // mount() gives us the VTree from the Haskell side, so we just attach it here
    // to tie the knot (attach to both vdom and real dom).
    obj.children.push(component);
    obj['domRef'].appendChild(component['domRef']);
  });
};
// creates nodes on virtual and dom (vtext, vcomp, vnode)
var create = function (obj: any, parent: any) {
  if (obj.type === 'vtext') {
    obj['domRef'] = document.createTextNode(obj['text']);
    parent.appendChild(obj['domRef']);
  } else {
    createElement(obj, function (ref: any) {
      parent.appendChild(ref);
    });
  }
};
/* Child reconciliation algorithm, inspired by kivi and Bobril */
var syncChildren = function (os: [any], ns: [any], parent: any) {
  var oldFirstIndex: number = 0,
    newFirstIndex: number = 0,
    oldLastIndex: number = os.length - 1,
    newLastIndex: number = ns.length - 1,
    nFirst: any,
    nLast: any,
    oLast: any,
    oFirst: any,
    tmp: any,
    found: boolean,
    node: any;
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
                   Otherwise, it will insert node['domRef'] before oLast['domRef']. */
      parent.insertBefore(nFirst['domRef'], oFirst ? oFirst['domRef'] : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    } /* No more new nodes, delete all remaining nodes in old list
               -> [ a b c ] <- old children
               -> [ ] <- new children
               */ else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        parent.removeChild(os[oldLastIndex--]['domRef']);
      }
      os.splice(oldFirstIndex, tmp - oldFirstIndex + 1);
      break;
    } /* happy path, everything aligns, we continue
               -> oldFirstIndex -> [ a b c ] <- oldLastIndex
               -> newFirstIndex -> [ a b c ] <- newLastIndex
               check if nFirst and oFirst align, if so, check nLast and oLast
               */ else if (oFirst['key'] === nFirst['key']) {
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent);
    } else if (oLast['key'] === nLast['key']) {
      diff(os[oldLastIndex--], ns[newLastIndex--], parent);
    } /* flip-flop case, nodes have been swapped, in some way or another
               both could have been swapped.
               -> [ a b c ] <- old children
               -> [ c b a ] <- new children
               */ else if (
      oFirst['key'] === nLast['key'] &&
      nFirst['key'] === oLast['key']
    ) {
      swapDomRefs(node, oLast['domRef'], oFirst['domRef'], parent);
      swap(os, oldFirstIndex, oldLastIndex);
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
                   and now we happy path
                   */ else if (oFirst['key'] === nLast['key']) {
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
               and now we happy path
               */ else if (oLast['key'] === nFirst['key']) {
      /* insertAfter */
      parent.insertBefore(oLast['domRef'], oFirst['domRef']);
      /* swap positions in old vdom */
      os.splice(oldFirstIndex, 0, os.splice(oldLastIndex, 1)[0]);
      diff(os[oldFirstIndex++], nFirst, parent);
      newFirstIndex++;
    } /* The 'you're screwed' case, nothing aligns, pull the ripcord, do something more fancy
               This can happen when the list is sorted, for example.
               -> [ a e c ] <- old children
               -> [ b e d ] <- new children
               */ else {
      /* final case, perform linear search to check if new key exists in old map, decide what to do from there */
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
                   In the above case 'b' has been moved, so we need to insert 'b' before 'a' in both vDOM and DOM
                   We also increase oldFirstIndex and newFirstIndex.
        
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
        createElement(nFirst, function (ref: any) {
          parent.insertBefore(ref, oFirst['domRef']);
        });
        os.splice(oldFirstIndex++, 0, nFirst);
        newFirstIndex++;
        oldLastIndex++;
      }
    }
  }
};
var swapDomRefs = function (tmp: any, a: any, b: any, p: any) {
  tmp = a.nextSibling;
  p.insertBefore(a, b);
  p.insertBefore(b, tmp);
};
var swap = function (os: any, l: any, r: any) {
  var k = os[l];
  os[l] = os[r];
  os[r] = k;
};
