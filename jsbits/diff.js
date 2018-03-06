/* virtual-dom diffing algorithm, applies patches as detected */
function diff (currentObj, newObj, parent, doc) {
  if (!currentObj && !newObj) return;
  else if (!currentObj && newObj) createNode (newObj, parent, doc);
  else if (currentObj && !newObj) destroyNode (currentObj, parent);
  else {
    if (currentObj.type === "vtext") {
      if (newObj.type === "vnode") replaceTextWithElement (currentObj, newObj, parent, doc);
      else diffTextNodes (currentObj, newObj);
    } else {
      if (newObj.type === "vnode") diffVNodes (currentObj, newObj, parent, doc);
      else replaceElementWithText (currentObj, newObj, parent, doc);
    }
  }
}

function destroyNode (obj, parent) {
    parent.removeChild (obj.domRef);
    callDestroyedRecursive(obj);
}

function callDestroyedRecursive (obj) {
  callDestroyed(obj);
  for (var i = 0; i < obj.children.length; i++)
    callDestroyedRecursive(obj.children[i]);
}

function callDestroyed (obj) {
  if (obj.onDestroyed) obj.onDestroyed();
}

function diffTextNodes (c, n) {
  if (c.text !== n.text) c.domRef.textContent = n.text;
  n.domRef = c.domRef;
}

function replaceElementWithText (c, n, parent, doc) {
  n.domRef = doc.createTextNode (n.text);
  parent.replaceChild (n.domRef, c.domRef);
  callDestroyedRecursive(c);
}

function replaceTextWithElement (c, n, parent, doc) {
  createElement (n, doc);
  parent.replaceChild (n.domRef, c.domRef);
  callCreated(n);
}

function callCreated (obj) {
  if (obj.onCreated) obj.onCreated();
}

function populate (c, n, doc) {
    if (!c) c = {
	      props : null
	    , css : null
	    , children : []
	    }
    diffProps (c.props, n.props, n.domRef, n.ns === "svg");
    diffCss (c.css, n.css, n.domRef);
    diffChildren (c.children, n.children, n.domRef, doc);
}

function diffVNodes (c, n, parent, doc) {
  if (c.tag === n.tag && n.key === c.key) {
    n.domRef = c.domRef;
    populate (c, n, doc);
  } else {
    createElement(n, doc);
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

function diffChildren (cs, ns, parent, doc) {
    var longest = ns.length > cs.length ? ns.length : cs.length;
    for (var i = 0; i < longest; i++)
      diff (cs[i], ns[i], parent, doc);
}

function createElement (obj, doc) {
    obj.domRef = obj.ns === "svg"
	? doc.createElementNS("http://www.w3.org/2000/svg", obj.tag)
	: doc.createElement(obj.tag);
    populate (null, obj, doc);
}

function createNode (obj, parent, doc) {
    if (obj.type === "vnode") createElement(obj, doc);
    else obj.domRef = doc.createTextNode(obj.text);
    parent.appendChild(obj.domRef);
    callCreated(obj);
}
