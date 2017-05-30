/* virtual-dom diffing algorithm, applies patches as detected */
function diff (currentObj, newObj, parent) {
  if (!currentObj && !newObj) return;
  else if (!currentObj && newObj) createNode (newObj, parent);
  else if (currentObj && !newObj) parent.removeChild (currentObj.domRef);
  else {
    if (currentObj.type === "vtext") {
      if (newObj.type === "vnode")
        replaceElementWithText (currentObj, newObj, parent);
      else
        diffTextNodes (currentObj, newObj);
    } else {
      if (newObj.type === "vnode")
        diffVNodes (currentObj, newObj, parent);
      else
        replaceTextWithElement (currentObj, newObj, parent);
    }
  }
}

function diffTextNodes (c, n) {
  if (c.text !== n.text)
    c.domRef.replaceData (0, c.domRef.length, n.text);
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
	       node[c] = null;
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
	    } else if (n in node) {
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
	  } else if (n in node) {
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

