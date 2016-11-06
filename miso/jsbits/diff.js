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
  /* check for svg here... make a render function? */
  n.domRef = document.createElement (n.tag);
  parent.replaceChild (n.domRef, c.domRef);
}

function populate (c, n) {
    if (!c) c = {
	      attrs : null
            , props : null
            , css : null
            , children : []
	    }
    if (!n) n = {
	      attrs : null
            , props : null
            , css : null
            , children : []
	    }
  diffAttrs (c.attrs, n.attrs, n.domRef, n.ns === "svg");
  diffProps (c.props, n.props, n.domRef);
  diffCss (c.css, n.css, n.domRef);
  diffChildren (c.children, n.children, n.domRef);
}

function diffVNodes (c, n, parent) {
  if (c.tag === n.tag) {
    n.domRef = c.domRef;
    populate (c, n);
  } else {
    /* check if svg here... render? */
    n.domRef = document.createElement(n.tag)
    populate (null, n);
    parent.replaceChild (n.domRef, c.domRef);
  }
}

function diffAttrs (cAttrs, nAttrs, node, isSVG) {
    var result;
    /* is current prop in new prop list? */
    for (var c in cAttrs) {
      result = nAttrs[c];
      if (!result) {
	/* current key is not in node */
	node.removeAttribute(c);
      } else {
       var domProp = node[c];
        if (result !== cAttrs[c] && result !== domProp) {
            if (c === "href" && isSVG)
              node.setAttributeNS("http://www.w3.org/1999/xlink", c, result);
	    else
	      node.setAttribute(c, result);
         }
      }
    }
    /* add remaining */
     for (var n in nAttrs) {
       if (cAttrs && cAttrs[n]) continue;
       if (n === "href" && isSVG)
          node.setAttributeNS("http://www.w3.org/1999/xlink", n, nAttrs[n]);
       else
          node.setAttribute(n, nAttrs[n]);
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

function diffProps (cProps, nProps, node) {
    var result;
    /* is current prop in new prop list? */
    for (var c in cProps) {
     result = nProps[c];
     if (!result) {
	/* current key is not in node */
       node[c] = null;
     } else {
       var domProp = node[c];
       if (result !== cProps[c] && result != domProp)
	node[c] = result;
     }
    }
    /* add remaining */
      for (var n in nProps) {
        if (cProps && cProps[n]) continue;
        node[n] = nProps[n];
   }
}

function diffChildren (cs, ns, parent) {
   //  if (!cs.length && !ns.length) return;
    var longest = ns.length > cs.length ? ns.length : cs.length;
    for (var i = 0; i < longest; i++)
      diff (cs[i], ns[i], parent);
}

function createNode (obj, parent) {
  if (obj.type === "vnode") {
      obj.domRef = obj.ns === "svg"
	  ? document.createElementNS("http://www.w3.org/2000/svg", obj.tag)
	  : document.createElement(obj.tag);
    populate (null, obj);
  } else {
    obj.domRef = document.createTextNode(obj.text);
  }
  parent.appendChild(obj.domRef);
}

