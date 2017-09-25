/* virtual-dom diffing algorithm, applies patches as detected */
function diff (currentObj, newObj, parent) {
  if (!currentObj && !newObj) return;
  else if (!currentObj && newObj) createNode (newObj, parent);
  else if (currentObj && !newObj) parent.removeChild (currentObj.domRef);
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
}

function replaceTextWithElement (c, n, parent) {
  createElement (n);
  parent.replaceChild (n.domRef, c.domRef);
}

function populate (c, n) {
  diffProps (c ? c.props : null, n.props, n.domRef, n.ns === "svg");
  diffCss (c ? c.css : null, n.css, n.domRef);
  diffChildren (c ? c.children : [], n.children, n.domRef);
}

function diffVNodes (c, n, parent) {
  if (c.tag === n.tag) {
    n.domRef = c.domRef;
    populate (c, n);
  } else {
    createElement(n);
    parent.replaceChild (n.domRef, c.domRef);
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
	     else if (c === "className" || c === "class")
		node.setAttributeNS("http://www.w3.org/1999/xlink", "class", newProp);
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
	    else if (n === "className" || n === "class")
	       node.setAttributeNS("http://www.w3.org/1999/xlink", "class", newProp);
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
    var longest = Math.max (ns.length, cs.length);
    if (hasKey(cs))
      syncChildren (cs, ns, parent);
    else
      for (var i = 0; i < longest; i++)
        diff (cs[i], ns[i], parent);
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
}

function createNodeDontAppend (obj) {
  if (obj.type === "vnode") createElement(obj);
  else obj.domRef = document.createTextNode(obj.text);
  return obj;
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
    , tmp
    , found
    , node;
    while (newFirstIndex <= newLastIndex || oldFirstIndex <= oldLastIndex) {
	 /* check base case, first > last for both new and old
	   [ ] -- old children empty (fully-swapped)
	   [ ] -- new children empty (fully-swapped)
	 */

	/* Initialize */
	nFirst = ns[newFirstIndex];
	nLast  = ns[newLastIndex];
	oFirst = os[oldFirstIndex];
	oLast  = os[oldLastIndex];

	/* No more old nodes, create and insert all remaining nodes
	   -> [ ] <- old children
	   -> [ a b c ] <- new children
	*/
	if (oldFirstIndex > oldLastIndex) {
	    createElement(nFirst);
	    /* insertBefore's semantics will append a node if the second argument provided is `null` or `undefined`.
	       Otherwise, it will insert node.domRef before oLast.domRef. */
            parent.insertBefore (nFirst.domRef, os[oldLastIndex] ? os[oldLastIndex].domRef.nextSibling : null);
	    newFirstIndex++;
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
	    } break;
	}
	/* happy path, everything aligns, we continue
	   -> oldFirstIndex -> [ a b c ] <- oldLastIndex
	   -> newFirstIndex -> [ a b c ] <- newLastIndex
	   check if nFirst and oFirst align, if so, check nLast and oLast
	*/
	else if (oFirst.key === nFirst.key || oLast.key === nLast.key) {
	    while (os[oldFirstIndex].key === ns[newFirstIndex].key) {
	      diff (os[oldFirstIndex], ns[newFirstIndex], parent);
 	      oldFirstIndex++; newFirstIndex++;
	      if (oldFirstIndex > oldLastIndex || newFirstIndex > newLastIndex) break;
	    }

            while (os[oldLastIndex].key === ns[newLastIndex].key) {
              diff (os[oldLastIndex], ns[newLastIndex], parent);
	      oldLastIndex--; newLastIndex--;
	      if (oldFirstIndex > oldLastIndex || newFirstIndex > newLastIndex) break;
	    }
        }
	/* flip-flop case, nodes have been swapped, in some way or another
	   both could have been swapped.
	   -> [ a b c ] <- old children
	   -> [ c b a ] <- new children
	*/
	else if (oFirst.key === nLast.key && nFirst.key === oLast.key) {
	    // swap array elements
	    tmp = oFirst;
	    oFirst = oLast;
	    oLast = tmp;

	    // swap pointers
	    node = oLast.domRef.nextSibling;
	    parent.insertBefore(oLast.domRef, oFirst.domRef.nextSibling);
	    parent.insertBefore(oFirst.domRef, node);

	    // diff
            diff (oLast, nLast, parent);
            diff (oFirst, nFirst, parent);

	    // incr/decr
	    newFirstIndex++; oldFirstIndex++;
	    oldLastIndex--; newLastIndex--;
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
        else if (oFirst.key === nLast.key) {
	    parent.insertBefore(oFirst.domRef, oLast.domRef.nextSibling);
	    /* swap positions in old vdom */
   	    os.splice(oldLastIndex, 0, os.splice(oldFirstIndex, 1)[0]);
	    diff (os[oldLastIndex], nLast, parent);
	    newLastIndex--; oldLastIndex--;
	}
	/*
           This is top right and bottom lefts match case.
           We move d to end of list, mutate old vdom to reflect the change
	   -> [ b a d ] <- old children
	   -> [ d b a ] <- new children
	   becomes
	   -> [ a b d ] <- old children
	   -> [ a b d ] <- new children
	   and now we happy path
	*/
        else if (oLast.key === nFirst.key) {
	    os.splice(oldFirstIndex, 0, os.splice(oldLastIndex, 1)[0]);
	    parent.insertBefore(oLast.domRef, oFirst.domRef);
	    diff (os[oldFirstIndex], nFirst, parent);
	    oldFirstIndex++; newFirstIndex++;
	}

	/* The "you're screwed" case, nothing aligns, pull the ripcord, do something more fancy
	   This can happen when the list is sorted, for example, or when the list is replaced with another list of differing keys.
	   -> [ a e c ] <- old children
	   -> [ b e d ] <- new children
	*/

	else {
	    /* final case, perform linear search to check if new key exists in old map, decide what to do from there */
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
	       In the above case 'b' has been moved, so we need to insert 'b' before 'a' in both vDOM and DOM
	       We also increase oldFirstIndex and newFirstIndex.

	       This results in new list below w/ updated index position
	       -> [ b a e c ] <- old children
	       -> [ b e a j ] <- new children
                      ^
   	    */
	    if (found) {
		/* Swap DOM references */
		parent.insertBefore(node.domRef, oFirst.domRef);
                /* Move item to correct position */
        	os.splice(oldFirstIndex, 0, os.splice(tmp, 1)[0]);
		/* optionally perform `diff` here */
                diff (os[oldFirstIndex], nFirst, parent);
		/* increment counters */
		oldFirstIndex++; newFirstIndex++;
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
		createElement(nFirst);
		parent.insertBefore(nFirst.domRef, oFirst.domRef);
		os.splice(oldFirstIndex, 0, nFirst);
		newFirstIndex++; oldFirstIndex++; oldLastIndex++;
	    }
	}
    }
}
