/* virtual-dom diffing algorithm, applies patches as detected */
function diff (currentObj, newObj, parent) {
  if (!currentObj && !newObj) return;
  else if (!currentObj && newObj) createNode (newObj, parent);
  else if (currentObj && !newObj) destroyNode(currentObj, parent);
  else {
    if (currentObj.type === "vtext") {
      if (newObj.type === "vnode") replaceTextWithElement (currentObj, newObj, parent);
      else diffTextNodes (currentObj, newObj);
    } else {
      if (newObj.type === "vnode") diffVNodes (currentObj, newObj, parent);
      else replaceElementWithText (currentObj, newObj, parent);
    }
  }
}

function diffTextNodes (c, n) {
  if (c.text !== n.text) c.domRef.textContent = n.text;
  n.domRef = c.domRef;
}

function replaceElementWithText (c, n, parent) {
  n.domRef = document.createTextNode (n.text);
  parent.replaceChild (n.domRef, c.domRef);
  callDestroyedRecursive(c);
}

function replaceTextWithElement (c, n, parent) {
  createElement (n);
  parent.replaceChild (n.domRef, c.domRef);
  callCreated(n);
}

function populate (c, n) {
    if (!c) c = {
	      props : null
	    , css : null
	    , children : []
	    }
    if (!n) n = {
	      props : null
	    , css : null
	    , children : []
	    }
  diffProps (c.props, n.props, n.domRef, n.ns === "svg");
  diffCss (c.css, n.css, n.domRef);
  diffChildren (c.children, n.children, n.domRef);
}

function diffVNodes (c, n, parent) {
  if (c.tag === n.tag && n.key === c.key) {
    n.domRef = c.domRef;
    populate (c, n);
  } else {
    createElement(n);
    parent.replaceChild (n.domRef, c.domRef);
    callDestroyedRecursive(c);
    callCreated(n);
  }
}

function diffProps (cProps, nProps, node, isSvg) {
    var result, newProp, domProp;
    /* Is current prop in new prop list? */
    for (var c in cProps) {
	newProp = nProps[c];
	 /* If current property no longer exists, remove it */
	 if (!newProp) {
	     /* current key is not in node, remove it from DOM, if SVG, remove attribute */
	     if (isSvg || !(c in node))
	       node.removeAttribute(c, cProps[c]);
	     else
	       node[c] = "";
	  } else {
	   /* Already on DOM from previous diff, continue */
	   if (newProp === cProps[c]) continue;
	   domProp = node[c];
	   /* Value in new prop map, not in old, but already pre-populated on DOM, potentially from server-prerendering, continue */
	   if (newProp === domProp) continue;
	   if (isSvg) {
	     if (c === "href")
		node.setAttributeNS("http://www.w3.org/1999/xlink", "href", newProp);
	     else
		node.setAttribute(c, newProp);
	    } else if (c in node && !(c === "list" || c === "form")) {
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
	    if (n === "href")
	       node.setAttributeNS("http://www.w3.org/1999/xlink", "href", newProp);
	    else
	       node.setAttribute(n, newProp);
	  } else if (n in node && !(n === "list" || n === "form")) {
	     node[n] = nProps[n];
	  } else {
	     node.setAttribute(n, newProp);
	 }
     }
}

function diffCss (cCss, nCss, node) {
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
}

function hasKey (cs) {
    return cs && cs[0] && cs[0].key;
}

function diffChildren (cs, ns, parent) {
    var longest = ns.length > cs.length ? ns.length : cs.length;
//    if (hasKey(cs)) syncChildren (cs, ns, parent);
    for (var i = 0; i < longest; i++) diff (cs[i], ns[i], parent);
}

function createElement (obj) {
    obj.domRef = obj.ns === "svg"
	? document.createElementNS("http://www.w3.org/2000/svg", obj.tag)
	: document.createElement(obj.tag);
    populate (null, obj);
}

function createNode (obj, parent) {
    if (obj.type === "vnode") createElement(obj);
    else obj.domRef = document.createTextNode(obj.text);
    parent.appendChild(obj.domRef);
    callCreated(obj);
}

function createNodeDontAppend (obj) {
  if (obj.type === "vnode") createElement(obj);
  else obj.domRef = document.createTextNode(obj.text);
  return obj;
}

function destroyNode (obj, parent) {
    parent.removeChild (obj.domRef);
    callDestroyedRecursive(obj);
}

function callDestroyed (obj) {
  if ("onDestroyed" in obj)
    obj.onDestroyed();
}

function callDestroyedRecursive (obj) {
  callDestroyed(obj);
  for (var i in obj.children)
    callDestroyedRecursive(obj.children[i]);
}

function callCreated (obj) {
  if ("onCreated" in obj)
    obj.onCreated();
}

/* Child reconciliation algorithm, inspired by kivi and Bobril */
function syncChildren (os, ns, parent) {
    var oldFirstIndex = 0
    , newFirstIndex = 0
    , oldLastIndex = os.length - 1
    , newLastIndex = ns.length - 1
    , nFirst
    , nLast
    , oFirst
    , oLast
    , LIS
    , temp;
    for (;;) {
	 /* check base case, first > last for both new and old
	   [ ] -- old children empty (fully-swapped)
	   [ ] -- new children empty (fully-swapped)
	 */
	if (newFirstIndex > newLastIndex && oldFirstIndex > oldLastIndex) break;
	  nFirst = ns[newFirstIndex];
	  nLast  = ns[newLastIndex];
	  oFirst = os[oldFirstIndex];
	  oLast  = os[oldLastIndex];
	/* No more old nodes, create and insert all remaining nodes
	   -> [ ] <- old children
	   -> [ a b c ] <- new children
	*/
	if (oldFirstIndex > oldLastIndex) {
	    var node = createNodeDontAppend(nFirst);
	    /* insertBefore's semantics will append a node if the second argument provided is `null` or `undefined`.
	       Otherwise, it will insert node.domRef before oLast.domRef. */
	    parent.insertBefore(node.domRef, oLast ? oLast.domRef : null);
	    os.splice(newFirstIndex, 0, nFirst);
	    newFirstIndex++;
	    continue;
	}
	/* No more new nodes, delete all remaining nodes in old list
	   -> [ a b c ] <- old children
	   -> [ ] <- new children
	*/
	else if (newFirstIndex > newLastIndex) {
	    tmp = oldLastIndex - oldFirstIndex;
	    while (tmp >= 0) {
	      parent.removeChild(os[oldFirstIndex].domRef);
	      os.splice(oldFirstIndex, 1);
	      tmp--;
	    }
	    break;
	}
	/* happy path, everything aligns, we continue
	   -> oldFirstIndex -> [ a b c ] <- oldLastIndex
	   -> newFirstIndex -> [ a b c ] <- newLastIndex
	   check if nFirst and oFirst align, if so, check nLast and oLast
	*/
	else if (oFirst.key === nFirst.key) {
	    newFirstIndex++;
	    oldFirstIndex++;
	    continue;
	} else if (oLast.key === nLast.key) {
	    newLastIndex--;
	    oldLastIndex--;
	    continue;
	}
	/* flip-flop case, nodes have been swapped, in some way or another
	   both could have been swapped.
	   -> [ a b c ] <- old children
	   -> [ c b a ] <- new children */
	else if (oFirst.key === nLast.key && nFirst.key === oLast.key) {
	    var nextSib = oFirst.domRef.nextSibling;
	    parent.insertBefore(oFirst.domRef, oLast.domRef);
	    parent.insertBefore(nextSib, oLast.domRef);
	    newFirstIndex++;
	    oldFirstIndex++;
	    oldLastIndex--;
	    newLastIndex--;
	    continue;
	}

	/* or just one could be swapped (d's align here)
	   -> [ d b g ] <- old children
	   -> [ a k d ] <- new children
	   on either side (e's align here)
	   -> [ e b c ] <- old children
	   -> [ b c e ] <- new children */

	/* For now, the above case is handled in the "you're screwed case" below. */

	/* The "you're screwed" case, nothing aligns, pull the ripcord, do something more fancy
	   This can happen when the list is sorted, for example.
	   -> [ a e c ] <- old children
	   -> [ b e d ] <- new children
	*/

	else {
	    var P = [], I = {}, i = 0, nLen = newLastIndex - newFirstIndex, oLen = oldLastIndex - oldFirstIndex,
		foundKey, last = -1, moved = false, newNodeIndex, removedNodes = 0;
	    /* Create array with length of new children list */
	    /* -1 means a new node should be inserted */
	    for (i = nLen; i > 0; i--) P.append(-1);
	    /* Create index I that maps keys with node positions of the remaining nodes from the new children */
	    for (i = newFirstIndex; i <= newLastIndex; i++) I[ns[i].key] = i;
	    /* Iterate over old nodes with Index, check if we can find node with same key in index */
	    for (i = oldFirstIndex; i <= oldLastIndex; i++) {
		node = os[i]; /* This will always return a match in this loop */
		newNodeIndex = I[node.key];
		/* If old node doesn't exist in new node list, remove it */
		if (!newNodeIndex) {
		    parent.removeChild(node.domRef);
		    removedNodes++;
		}
		/* Found new node in index map */
		else {
		    /* Assign position of the node in the old children list to the positions array. */
		    P[newNodeIndex] = i;
		    /* When assigning positions in the positions array, we also keep the last seen node position of the new children
		       list. If the last seen position is larger than current position of the node at the new list, then we are switching
		       `moved` flag to `true`. */

		    /* First check if last seen node position is larger than current node position */
		    if (last > newNodeIndex) moved = true;
		    /* Update last seen */
		    last = newNodeIndex;
		}
	    }
	    /* If `moved` flag is on, or if the length of the old children list minus the number of
	       removed nodes isn't equal to the length of the new children list. Then go to the next step */
	    if (moved /* DMJ: not sure we need this predicate ? ===> */ || (oLen - removedNodes !== nLen)) {
		/* Find minimum number of moves if `moved` flag is on, or insert new nodes if the length is changed. */
		LIS = lis(P);
		lisIndex = LIS.length - 1;
		while (nLen > -1) {
		    if (P[lisIndex] === nLen) {
			listIndex--;
			continue;
		    }
		    else if (LIS[lisIndex] !== nLen) {
			/* node has moved */
			ns[nLen].domRef = os[P[nLen]].domRef;
			parent.insertBefore(ns[nLen].domRef, os[nLen].domRef);
		    }
		    nLen--;
		}
	    } else if (!moved) {
		/* When moved flag is off, we don't need to find LIS, and we just
		   iterate over the new children list and check its
		   current position in the positions array, if it is `-1`,
		   then we insert new node. */
		for (i = newFirstIndex; i <= newLastIndex; i++) {
		    if (P[i] === -1) {
			createNodeDontAppend(ns[i]);
			/* Replace whatever is at current */
			/* parent.replaceChild(ns[i].domRef, parent.children[i]); ... this doesn't seem right to me... */
			parent.insertBefore(ns[i].domRef, parent.children[i+1]);
		    }
		}
	    }
	}
    }
}


/* Thanks Boris :) */
function lis(a) {
  var p = a.slice(0);
  var result = [], u, v, il, c, j;
  result.push(0);

  for (var i = 0, il = a.length; i < il; i++) {
    if (a[i] === -1) {
      continue;
    }

    j = result[result.length - 1];
    if (a[j] < a[i]) {
      p[i] = j;
      result.push(i);
      continue;
    }

    u = 0;
    v = result.length - 1;

    while (u < v) {
      c = ((u + v) / 2) | 0;
      if (a[result[c]] < a[i]) {
	u = c + 1;
      } else {
	v = c;
      }
    }

    if (a[i] < a[result[u]]) {
      if (u > 0) {
	p[i] = result[u - 1];
      }
      result[u] = i;
    }
  }

  u = result.length;
  v = result[u - 1];

  while (u-- > 0) {
    result[u] = v;
    v = p[v];
  }

  return result;
}
