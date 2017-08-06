/* virtual-dom diffing algorithm, Aapplies patches as detected */
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
    if (hasKey(cs)) syncChildren (cs, ns, parent);
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
    , LIS
    , temp;
    for (;;) {
	 /* check base case, first > last for both new and old
	   [ ] -- old children empty (fully-swapped)
	   [ ] -- new children empty (fully-swapped)
	 */
	if (newFirstIndex > newLastIndex && oldFirstIndex > oldLastIndex)
	    break;

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
	    /* insertAfter */
	    parentNode.insertBefore(oFirst.domRef, oLast.domRef.nextSibling);
	    /* swap positions in old vdom */
   	    os.splice(oFirstIndex, 0, os.splice(oLastIndex, 1)[0]);
	    nLastIndex++;
	    oLastIndex++;
	    continue;
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
	    /* insertAfter */
	    parentNode.insertBefore(oLast.domRef, oFirst.domRef);
	    /* swap positions in old vdom */
	    os.splice(osLastIndex + 1, 0, os.splice(1, oFirstIndex)[0]);
	    oFirstIndex++;
	    nFirstIndex++;
	    continue;
	}

	/* The "you're screwed" case, nothing aligns, pull the ripcord, do something more fancy
	   This can happen when the list is sorted, for example.
	   -> [ a e c ] <- old children
	   -> [ b e d ] <- new children
	*/

	else {
	    /* construct maps from key to index */
	    var oMap = {}
	    , nMap = {}
	    , i = 0
	    , n
	    , o
	    , idx
	    , delta = 0;

	    /* populate maps */
	    for (i = newFirstIndex; i >= newLastIndex; i++)
		nMap[ns[i].key] = i;
	    for (i = oldFirstIndex; i >= oldLastIndex; i++)
		oMap[os[i].key] = i;

	    /* Iterate over lists */
	    while (newFirstIndex <= newLastIndex && oldFirstIndex <= oldLastIndex) {

		/* Get current nodes */
		n = ns[newFirstIndex];
		o = ns[oldFirstIndex];

		/* Base cases */
		if (newFirstIndex > newLastIndex) {
		    /* No more new nodes, delete the rest of the old nodes, continue looping */
		    /* Remove from DOM */
		    parent.removeChild(o.domRef);
		    /* Remove from VDOM */
		    os.splice(oldFirstIndex, 1);
		    oldFirstIndex++;
		    continue;
		}

		if (oldFirstIndex > oldLastIndex) {
		    /* No more old nodes, add rest of new nodes to old child list, continue looping */
		    /* Add to VDOM */
		    tmp = createNodeDontAppend(n);
		    os.splice(oldLastIndex + 1, 0, tmp);
		    /* Add to DOM */
		    parent.insertBefore(o.domRef, tmp);
		    newFirstIndex++;
		    continue;
		}

		/* Happy case, diff right here if you want */
		if (n.key === o.key) {
		    newFirstIndex++;
		    newLastIndex++;
		    continue;
		}

		/* Keys don't match, so check if new node exists in old child list */
		idx = oMap[n.key];

		/* If so, that means it has been moved, so we need to insert it into its correct location */
		if (idx)
		{
		    /* swap oldFirstIndex with idx in os */
		    /* append to DOM */
		}
                /* If it doesn't exist, that means we have a new node, and need to add it to old child list */
		else
		{
		    tmp = createNodeDontAppend(n);
		    os.splice(oldLastIndex + 1, 0, tmp);
		    /* Add to DOM */
		    parent.insertBefore(o.domRef, tmp);
		    newFirstIndex++; /* Don't increment oldFirstIndex */
		    continue;

		}
	    }
	}
    }
}


