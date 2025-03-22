/* virtual-dom diffing algorithm, applies patches as detected */
window['diff'] = function (currentObj, newObj, parent, doc) {
  if (!currentObj && !newObj) return;
  else if (!currentObj && newObj) window['create'](newObj, parent, doc);
  else if (currentObj && !newObj) window['destroy'](currentObj, parent);
  else {
      if (currentObj['type'] === newObj['type'])
        window['diffNodes'](currentObj, newObj, parent, doc);
      else
        window['replace'](currentObj, newObj, parent, doc);
  }
};

// replace everything function
window['replace'] = function (c, n, parent, doc) {
  // step1 : prepare to delete, unmount things
  window['callBeforeDestroyedRecursive'](c);
  // ^ this will unmount sub components before we replace the child
  // and will recursively call hooks on nodes
  // step2 : create new things, replace old things with new things
  if (n['type'] === 'vtext') {
    n['domRef'] = doc.createTextNode(n['text']);
    parent.replaceChild(n['domRef'], c['domRef']);
  } else {
    window['createElement'](n, doc, function(ref) {
      parent.replaceChild(ref, c['domRef']);
    });
  }
  // step 3: call destroyed hooks, call created hooks
  window['callDestroyedRecursive'](c);
}

// destroy vtext, vnode, vcomp
window['destroy'] = function (obj, parent) {
  // step 1: invoke destroy pre-hooks on vnode and vcomp
  window['callBeforeDestroyedRecursive'](obj);

  // step 2: destroy
  parent.removeChild(obj['domRef']);

  // step 3: invoke post-hooks for vnode and vcomp
  window['callDestroyedRecursive'](obj);
};

window['diffNodes'] = function (c, n, parent, doc) {
  // bail out on easy vtext case
  if (c['type'] === 'vtext') {
      if (c['text'] !== n['text']) c['domRef'].textContent = n['text'];
      n['domRef'] = c['domRef'];
      return;
  }
  // check children
  if (c['tag'] === n['tag'] && n['key'] === c['key'] && n['data-component-id'] === c['data-component-id']) {
      n['domRef'] = c['domRef'];
      // dmj: we will diff properties on 'vcomp' as well
      window['populate'](c, n, doc);
  } else {
    // dmj: we replace when things just don't line up during the diff
    window['replace'](c,n,parent,doc);
  }
};

// ** recursive calls to hooks
window['callDestroyedRecursive'] = function (obj) {
  window['callDestroyed'](obj);
  for (var i in obj.children)
    window['callDestroyedRecursive'](obj.children[i]);
};

window['callDestroyed'] = function (obj) {
  if (obj['onDestroyed']) obj['onDestroyed']();
};

window['callBeforeDestroyed'] = function (obj) {
  if (obj['onBeforeDestroyed']) obj['onBeforeDestroyed']();
  if (obj['type'] === 'vcomp') obj['unmount'](obj.domRef);
};

window['callBeforeDestroyedRecursive'] = function (obj) {
    window['callBeforeDestroyed'](obj);
    for (var i in obj.children) {
        window['callBeforeDestroyedRecursive'](obj.children[i]);
    }
};
// ** </> recursive calls to hooks
window['callCreated'] = function (obj, doc) {
  if (obj['onCreated']) obj['onCreated']();
  if (obj['type'] === 'vcomp') window['mountComponent'](obj, doc);
};

window['populate'] = function (c, n, doc) {
  if (!c) c = {
    props: null,
    css: null,
    children: []
  }
  window['diffProps'](c['props'], n['props'], n['domRef'], n['ns'] === 'svg');
  window['diffCss'](c['css'], n['css'], n['domRef']);
  if (n['type'] === 'vcomp') return; // dmj: don't diff vcomp children
  window['diffChildren'](c['children'], n['children'], n['domRef'], doc);
};

window['diffProps'] = function (cProps, nProps, node, isSvg) {
  var newProp;
  /* Is current prop in new prop list? */
  for (var c in cProps) {
    newProp = nProps[c];
    /* If current property no longer exists, remove it */
    if (newProp === undefined) {
      /* current key is not in node, remove it from DOM, if SVG, remove attribute */
      if (isSvg || !(c in node))
        node.removeAttribute(c, cProps[c]);
      else
        node[c] = '';
    } else {
      /* Already on DOM from previous diff, continue */
      if (newProp === cProps[c] && c !== 'checked' && c !== 'value') continue;
      if (isSvg) {
        if (c === 'href')
          node.setAttributeNS('http://www.w3.org/1999/xlink', 'href', newProp);
        else
          node.setAttribute(c, newProp);
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
      if (n === 'href')
        node.setAttributeNS('http://www.w3.org/1999/xlink', 'href', newProp);
      else
        node.setAttribute(n, newProp);
    } else if (n in node && !(n === 'list' || n === 'form')) {
      node[n] = nProps[n];
    } else {
      node.setAttribute(n, newProp);
    }
  }
};

window['diffCss'] = function (cCss, nCss, node) {
  var result;
  /* is current attribute in new attribute list? */
  for (var c in cCss) {
    result = nCss[c];
    if (!result) {
      /* current key is not in node */
      node.style[c] = null;
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

window['hasKeys'] = function (ns, cs) {
  return ns.length > 0 && cs.length > 0 && ns[0]['key'] != null && cs[0]['key'] != null;
};

window['diffChildren'] = function diffChildren(cs, ns, parent, doc) {
  var longest = ns.length > cs.length ? ns.length : cs.length;
  if (window['hasKeys'](ns, cs)) {
    window['syncChildren'](cs, ns, parent, doc);
  } else {
    for (var i = 0; i < longest; i++)
      window['diff'](cs[i], ns[i], parent, doc);
  }
};

window['populateDomRef'] = function (obj, doc) {
  if (obj['ns'] === 'svg') {
    obj['domRef'] = doc.createElementNS('http://www.w3.org/2000/svg', obj['tag']);
  } else if (obj['ns'] === 'mathml') {
    obj['domRef'] = doc.createElementNS('http://www.w3.org/1998/Math/MathML', obj['tag']);
  } else {
    obj['domRef'] = doc.createElement(obj['tag']);
  }
}

// dmj: refactor this, the callback function feels meh
window['createElement'] = function (obj, doc, cb) {
  window['populateDomRef'](obj,doc);
  cb(obj['domRef']);
  window['populate'](null, obj, doc);
  window['callCreated'](obj, doc);
};

// mounts vcomp by calling into Haskell side.
// unmount is handled with pre-destroy recursive hooks
window['mountComponent'] = function (obj, doc) {
    var componentId = obj['data-component-id'],
        nodeList = doc.querySelectorAll ("[data-component-id='" + componentId + "']");

    // dmj: bail out if duplicate mounting detected
    if (nodeList.length > 0) {
        console.error
          ('AlreadyMountedException: Component "' + componentId + "' is already mounted");
        return;
    }
    // dmj, the component placeholder div[id=name] is already on the dom and vdom
    // Now we gen the component and append it to the vdom and real dom
    obj['domRef'].setAttribute('data-component-id', componentId);
    // ^ we have to set this before 'mount()' is called, since `diff` requires it.
    obj['mount'](function(component) {
      // mount() gives us the VTree from the Haskell side, so we just attach it here
      // to tie the knot (attach to both vdom and real dom).
      obj.children.push(component);
      obj['domRef'].appendChild(component['domRef']);
    });
}

// creates nodes on virtual and dom (vtext, vcomp, vnode)
window['create'] = function (obj, parent, doc) {
  if (obj.type === 'vtext') {
      obj['domRef'] = doc.createTextNode(obj['text']);
      parent.appendChild(obj['domRef']);
  }
  else {
      window['createElement'](obj, doc, function (ref) {
        parent.appendChild(ref);
      });
  }
};

/* Child reconciliation algorithm, inspired by kivi and Bobril */
window['syncChildren'] = function(os, ns, parent, doc) {
  var oldFirstIndex = 0,
    newFirstIndex = 0,
    oldLastIndex = os.length - 1,
    newLastIndex = ns.length - 1,
    nFirst, nLast, oLast, oFirst, tmp, found, node;
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
      window['diff'](null, nFirst, parent, doc);
      /* insertBefore's semantics will append a node if the second argument provided is `null` or `undefined`.
         Otherwise, it will insert node['domRef'] before oLast['domRef']. */
      parent.insertBefore(nFirst['domRef'], oFirst ? oFirst['domRef'] : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    }
    /* No more new nodes, delete all remaining nodes in old list
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
    /* happy path, everything aligns, we continue
       -> oldFirstIndex -> [ a b c ] <- oldLastIndex
       -> newFirstIndex -> [ a b c ] <- newLastIndex
       check if nFirst and oFirst align, if so, check nLast and oLast
       */
    else if (oFirst['key'] === nFirst['key']) {
      window['diff'](os[oldFirstIndex++], ns[newFirstIndex++], parent, doc);
    } else if (oLast['key'] === nLast['key']) {
      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);
    }
    /* flip-flop case, nodes have been swapped, in some way or another
       both could have been swapped.
       -> [ a b c ] <- old children
       -> [ c b a ] <- new children
       */
    else if (oFirst['key'] === nLast['key'] && nFirst['key'] === oLast['key']) {
      window['swapDomRefs'](node, oLast['domRef'], oFirst['domRef'], parent);
      window['swap'](os, oldFirstIndex, oldLastIndex);
      window['diff'](os[oldFirstIndex++], ns[newFirstIndex++], parent, doc);
      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);
    }
    /* Or just one could be swapped (d's align here)
           This is top left and bottom right match case.
           We move d to end of list, mutate old vdom to reflect the change
           We then continue without affecting indexes, hoping to land in a better case
           -> [ d a b ] <- old children
           -> [ a b d ] <- new children
           becomes
           -> [ a b d ] <- old children
           -> [ a b d ] <- new children
           and now we happy path
           */
    else if (oFirst['key'] === nLast['key']) {
      /* insertAfter */
      parent.insertBefore(oFirst['domRef'], oLast['domRef'].nextSibling);
      /* swap positions in old vdom */
      os.splice(oldLastIndex,0,os.splice(oldFirstIndex,1)[0]);
      window['diff'](os[oldLastIndex--], ns[newLastIndex--], parent, doc);
    }
    /* This is top right and bottom lefts match case.
       We move d to end of list, mutate old vdom to reflect the change
       -> [ b a d ] <- old children
       -> [ d b a ] <- new children
       becomes
       -> [ d b a ] <- old children
       -> [ d b a ] <- new children
       and now we happy path
       */
    else if (oLast['key'] === nFirst['key']) {
      /* insertAfter */
      parent.insertBefore(oLast['domRef'], oFirst['domRef']);
      /* swap positions in old vdom */
      os.splice(oldFirstIndex,0, os.splice(oldLastIndex,1)[0]);
      window['diff'](os[oldFirstIndex++], nFirst, parent, doc);
      newFirstIndex++;
    }

    /* The 'you're screwed' case, nothing aligns, pull the ripcord, do something more fancy
       This can happen when the list is sorted, for example.
       -> [ a e c ] <- old children
       -> [ b e d ] <- new children
       */
    else {
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
        os.splice(oldFirstIndex,0, os.splice(tmp,1)[0]);
        /* optionally perform `diff` here */
        window['diff'](os[oldFirstIndex++], nFirst, parent, doc);
        /* Swap DOM references */
        parent.insertBefore(node['domRef'], os[oldFirstIndex]['domRef']);
        /* increment counters */
        newFirstIndex++;
      }
      /* If new key was *not* found in the old map this means it must now be created, example below
           -> [ a e d c ] <- old children
           -> [ b e a j ] <- new children
            ^

           In the above case 'b' does not exist in the old map, so we create a new element and DOM reference.
           We then insertBefore in both vDOM and DOM.

           -> [ b a e d c ] <- old children
           -> [ b e a j   ] <- new children
              ^
              */
      else {
        window['createElement'](nFirst, doc, function (ref) {
           parent.insertBefore(ref, oFirst['domRef']);
        });
        os.splice(oldFirstIndex++, 0, nFirst);
        newFirstIndex++;
        oldLastIndex++;
      }
    }
  }
};

window['swapDomRefs'] = function (tmp,a,b,p) {
  tmp = a.nextSibling;
  p.insertBefore(a,b);
  p.insertBefore(b,tmp);
};

window['swap']= function (os,l,r) {
  var k = os[l];
  os[l] = os[r];
  os[r] = k;
};

/* event delegation algorithm */
window['delegate'] = function (mount, events, getVTree, debug) {
  for (var event in events)
    mount.addEventListener
      ( events[event][0]
      , function (e) { window['listener'](e, mount, getVTree, debug); }
      , events[event][1]
      );
};

/* the event listener shared by both delegator and undelegator */
window['listener'] = function(e, mount, getVTree, debug) {
    getVTree(function (obj) {
        if (e.target) {
            window['delegateEvent'](e, obj, window['buildTargetToElement'](mount, e.target), [], debug);
        }
   });
}

/* event undelegation */
window['undelegate'] = function (mount, events, getVTree, debug) {
  for (var event in events)
    mount.removeEventListener
      ( events[event][0]
      , function (e) { window['listener'](e, mount, getVTree, debug); }
      , events[event][1]
      );
};

/* Finds event in virtual dom via pointer equality
   Accumulate parent stack as well for propagation up the vtree
 */
window['delegateEvent'] = function (event, obj, stack, parentStack, debug) {

  /* base case, not found */
  if (!stack.length) {
     if (debug) {
       console.warn('Event "' + event.type + '" did not find an event handler to dispatch on', obj, event);
     }
     return;
  }

  /* stack not length 1, recurse */
  else if (stack.length > 1) {
    parentStack.unshift(obj);
    for (var o = 0; o < obj.children.length; o++) {
      if (obj['type'] === 'vcomp') continue;
      if (obj.children[o]['domRef'] === stack[1]) {
        window['delegateEvent'](event, obj.children[o], stack.slice(1), parentStack, debug);
        break;
      }
    }
  }

  /* stack.length == 1 */
  else {
    var eventObj = obj['events'][event.type];
    if (eventObj) {
      var options = eventObj['options'];
      if (options['preventDefault'])
        event.preventDefault();
      eventObj['runEvent'](event);
      if (!options['stopPropagation'])
        window['propagateWhileAble'] (parentStack, event);
    } else {
      /* still propagate to parent handlers even if event not defined */
      window['propagateWhileAble'] (parentStack, event);
    }
  }
};

/* Create a stack of ancestors used to index into the virtual DOM */
window['buildTargetToElement'] = function buildTargetToElement (element, target) {
  var stack = [];
  while (element !== target) {
    stack.unshift (target);
    target = target.parentNode;
  }
  return stack;
};

/* Propagate the event up the chain, invoking other event handlers as encountered */
window['propagateWhileAble'] = function propagateWhileAble (parentStack, event) {
  for (var i = 0; i < parentStack.length; i++) {
    if (parentStack[i]['events'][event.type]) {
      var eventObj = parentStack[i]['events'][event.type], options = eventObj['options'];
      if (options['preventDefault']) event.preventDefault();
      eventObj['runEvent'](event);
      if (options['stopPropagation']) {
        event.stopPropagation();
        break;
      }
    }
  }
};

/* Walks down obj following the path described by `at`, then filters primitive
   values (string, numbers and booleans). Sort of like JSON.stringify(), but
   on an Event that is stripped of impure references.
*/
window['eventJSON'] = function eventJSON (at, obj) {
  /* If at is of type [[MisoString]] */
  if (typeof at[0] == 'object') {
    var ret = [];
    for (var i = 0; i < at.length; i++)
      ret.push(window['eventJSON'](at[i], obj));
    return ret;
  }

  for (var i in at) obj = obj[at[i]];

  /* If obj is a list-like object */
  var newObj;
  if (obj instanceof Array || ('length' in obj && obj['localName'] !== 'select')) {
    newObj = [];
    for (var i = 0; i < obj.length; i++)
      newObj.push(window['eventJSON']([], obj[i]));
    return newObj;
  }

  /* If obj is a non-list-like object */
  newObj = {};
  for (var i in getAllPropertyNames(obj)){
    /* bug in safari, throws TypeError if the following fields are referenced on a checkbox */
    /* https://stackoverflow.com/a/25569117/453261 */
    /* https://html.spec.whatwg.org/multipage/input.html#do-not-apply */
    if ((obj['localName'] === 'input') && (i === 'selectionDirection' || i === 'selectionStart' || i === 'selectionEnd'))
      continue;
    if (typeof obj[i] == 'string' || typeof obj[i] == 'number' || typeof obj[i] == 'boolean')
      newObj[i] = obj[i];
  }
  return newObj;
};

/* get static and dynamic properties */
function getAllPropertyNames(obj) {
  var props = {}, i = 0;
  do {
    var names = Object.getOwnPropertyNames(obj);
    for (i = 0; i < names.length; i++) {
      props [names[i]] = null;
    }
  } while (obj = Object.getPrototypeOf(obj));
  return props;
};

/* prerendering / hydration / isomorphic support */
window['collapseSiblingTextNodes'] = function collapseSiblingTextNodes(vs) {
  var ax = 0, adjusted = vs.length > 0 ? [vs[0]] : [];
  for (var ix = 1; ix < vs.length; ix++) {
    if (adjusted[ax]['type'] === 'vtext' && vs[ix]['type'] === 'vtext') {
      adjusted[ax]['text'] += vs[ix]['text'];
      continue;
    }
    adjusted[++ax] = vs[ix];
  }
  return adjusted;
}

window['copyDOMIntoVTree'] = function copyDOMIntoVTree(logLevel,mountPoint,vtree,doc) {
  var mountChildIdx = 0, node;
  // If script tags are rendered first in body, skip them.
  if (!mountPoint) {
    if (doc.body.childNodes.length > 0) {
      node = doc.body.firstChild;
    } else {
      node = doc.body.appendChild (doc.createElement('div'));
    }
  } else if (mountPoint.childNodes.length === 0) {
    node = mountPoint.appendChild (doc.createElement('div'));
  } else {
    while (mountPoint.childNodes[mountChildIdx] && (mountPoint.childNodes[mountChildIdx].nodeType === Node.TEXT_NODE || mountPoint.childNodes[mountChildIdx].localName === 'script')){
      mountChildIdx++;
    }
    if (!mountPoint.childNodes[mountChildIdx]) {
      node = doc.body.appendChild (doc.createElement('div'));
    } else {
      node = mountPoint.childNodes[mountChildIdx];
    }
  }

  if (!window['walk'](logLevel,vtree, node, doc)) {
    if (logLevel) {
      console.warn('Could not copy DOM into virtual DOM, falling back to diff');
    }
    // Remove all children before rebuilding DOM
    while (node.firstChild) node.removeChild(node.lastChild);
    vtree['domRef'] = node;
    window['populate'](null, vtree, doc);
    return false;
  } else {
    if (logLevel) {
      var result = window['integrityCheck'](true, vtree);
      if (!result) {
          console.warn ('Integrity check completed with errors');
      } else {
          console.info ('Successfully prerendered page');
      }
    }
  }
  return true;
}

window['diagnoseError'] = function diagnoseError(logLevel, vtree, node) {
  if (logLevel) console.warn('VTree differed from node', vtree, node);
}

// https://stackoverflow.com/questions/11068240/what-is-the-most-efficient-way-to-parse-a-css-color-in-javascript
window['parseColor'] = function(input) {
    if (input.substr(0,1)=="#") {
    var collen=(input.length-1)/3;
    var fact=[17,1,0.062272][collen-1];
    return [
        Math.round(parseInt(input.substr(1,collen),16)*fact),
        Math.round(parseInt(input.substr(1+collen,collen),16)*fact),
        Math.round(parseInt(input.substr(1+2*collen,collen),16)*fact)
    ];
    }
    else return input.split("(")[1].split(")")[0].split(",").map(x=>+x);
}

// dmj: Does deep equivalence check, spine and leaves of virtual DOM to DOM.
window['integrityCheck'] = function (result, vtree) {
    // text nodes must be the same
    if (vtree['type'] == 'vtext') {
        if (vtree['domRef'].nodeType !== Node.TEXT_NODE) {
            console.warn ('VText domRef not a TEXT_NODE', vtree);
            result = false;
        }
        else if (vtree['text'] !== vtree['domRef'].textContent) {
            console.warn ('VText node content differs', vtree);
            result = false;
        }
    }

    // if vnode / vcomp, must be the same
    else {

        // tags must be identical
        if (vtree['tag'].toUpperCase() !== vtree['domRef'].tagName) {
            console.warn ('Integrity check failed, tags differ', vtree['tag'].toUpperCase(), vtree['domRef'].tagName);
            result = false;
        }
        // Child lengths must be identical
        if ('children' in vtree && vtree['children'].length !== vtree['domRef'].childNodes.length) {
            console.warn ('Integrity check failed, children lengths differ', vtree, vtree.children, vtree['domRef'].childNodes);
            result = false;
        }

        // properties must be identical
        var keyLength = Object.keys(vtree['props']).length; key = null;
        for (var i = 0; i < keyLength; i++) {
            key = Object.keys(vtree['props'])[i];
            if (key === 'href') {
                var absolute = window.location.origin + '/' + vtree['props'][key],
                    url = vtree['domRef'][key],
                    relative = vtree['props'][key];
                if (absolute !== url && relative !== url && (relative + '/') !== url && (absolute + '/') !== url) {
                    console.warn ('Property ' + key + ' differs', vtree['props'][key], vtree['domRef'][key]);
                    result = false;
                }
            }
            else if (key === 'height' || key === 'width') {
                if (parseFloat(vtree['props'][key]) !== parseFloat(vtree['domRef'][key])) {
                    console.warn ('Property ' + key + ' differs', vtree['props'][key], vtree['domRef'][key]);
                    result = false;
                }
            }
            else if (key === 'class' || key === 'className') {
                if (vtree['props'][key] !== vtree['domRef'].className) {
                    console.warn ('Property class differs', vtree['props'][key], vtree['domRef'].className)
                    result = false;
                }
            } else if (!vtree['domRef'][key]) {
                if (vtree['props'][key] !== vtree['domRef'].getAttribute(key)) {
                    console.warn ('Property ' + key + ' differs', vtree['props'][key], vtree['domRef'].getAttribute(key));
                    result = false;
                }
            }
            else if (vtree['props'][key] !== vtree['domRef'][key]) {
                console.warn ('Property ' + key + ' differs', vtree['props'][key], vtree['domRef'][key]);
                result = false;
            }
        }

        // styles must be identical
        keyLength = Object.keys(vtree['css']).length;
        for (var i = 0; i < keyLength; i++) {
            key = Object.keys(vtree['css'])[i];
            if (key === 'color') {
                if (window['parseColor'](vtree['domRef'].style[key]).toString() !== window['parseColor'](vtree['css'][key]).toString()) {
                    console.warn ('Style ' + key + ' differs', vtree['css'][key], vtree['domRef'].style[key]);
                    result = false;
                }
            }
            else if (vtree['css'][key] !== vtree['domRef'].style[key]) {
                console.warn ('Style ' + key + ' differs', vtree['css'][key], vtree['domRef'].style[key]);
                result = false;
            }
        }

        // recursive call for `vnode` / `vcomp`
        for (var i = 0; i < vtree.children.length; i++) {
            result &= window['integrityCheck'](result, vtree.children[i]);
        }
    }
    return result;
}


window['walk'] = function walk(logLevel, vtree, node, doc) {
  // This is slightly more complicated than one might expect since
  // browsers will collapse consecutive text nodes into a single text node.
  // There can thus be fewer DOM nodes than VDOM nodes.
  // We handle this in collapseSiblingTextNodes
  var vdomChild, domChild;
  vtree['domRef'] = node;

  // Fire onCreated events as though the elements had just been created.
  window['callCreated'](vtree);

  vtree.children = window['collapseSiblingTextNodes'](vtree.children);

  for (var i = 0; i < vtree.children.length; i++) {
    vdomChild = vtree['children'][i];
    domChild = node.childNodes[i];
    if (!domChild) {
      window['diagnoseError'](logLevel,vdomChild, domChild);
      return false;
    }
    if (vdomChild.type === 'vtext') {
      if (domChild.nodeType !== Node.TEXT_NODE) {
        window['diagnoseError'](logLevel, vdomChild, domChild);
        return false;
      }

      if (vdomChild['text'] === domChild.textContent) {
        vdomChild['domRef'] = domChild;
      } else {
        window['diagnoseError'](logLevel, vdomChild, domChild);
        return false;
      }
    } else if (vdomChild['type'] === 'vcomp') {
        vdomChild['mount'](function(component) {
           vdomChild.children.push(component);
           window['walk'](logLevel, vdomChild, domChild, doc);
        });
    } else {
      if (domChild.nodeType !== Node.ELEMENT_NODE) return false;
      vdomChild['domRef'] = domChild;
      window['walk'](logLevel, vdomChild, domChild, doc);
    }
  }
  return true;
}

/* various utilities */
window['callFocus'] = function (id) {
  setTimeout(function(){
    var ele = document.getElementById(id);
    if (ele && ele.focus) ele.focus()
  }, 50);
}

window['callBlur'] = function (id) {
  setTimeout(function(){
    var ele = document.getElementById(id);
    if (ele && ele.blur) ele.blur()
  }, 50);
}

window['setBodyComponent'] = function (componentId) {
   document.body.setAttribute('data-component-id', componentId);
}

/* exports */
var module = module || {};
module.exports = {
  callFocus : window['callFocus'],
  callBlur : window['callBlur'],
  diff : window['diff'],
  copyDOMIntoVTree : window['copyDOMIntoVTree'],
  integrityCheck : window['integrityCheck'],
  delegate : window['delegate'],
  undelegate : window['undelegate'],
  eventJSON : window['eventJSON']
};
