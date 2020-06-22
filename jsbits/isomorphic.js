window = typeof window === 'undefined' ? {} : window;
window['collapseSiblingTextNodes'] = function collapseSiblingTextNodes(vs) {
  if (!vs) { return []; }
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

window['copyDOMIntoVTree'] = function copyDOMIntoVTree(debug,mountPoint, vtree, doc) {
  if (!doc) { doc = window.document; }
  var mountChildIdx = 0;
  // If script tags are rendered first in body, skip them.
  while (mountPoint && mountPoint.childNodes && mountPoint.childNodes[mountChildIdx].localName === 'script'){
    mountChildIdx++;
  }
  var node = mountPoint && mountPoint.childNodes ? mountPoint.childNodes [mountChildIdx] : doc.body.firstChild;
    if (!window['walk'](debug,vtree, node, doc)) {
    if (debug) {
      console.warn('Could not copy DOM into virtual DOM, falling back to diff');
    }
    // Remove all children before rebuilding DOM
    while (node.firstChild) node.removeChild(node.lastChild);
    // Move node to end since diffing begins at last node of mount point.
    // No-op if no other nodes are children of body.
    node.parentNode.appendChild (node);
    window['diff'](null, vtree, node.parentNode, doc);
    return false;
  }
  if (debug) {
    console.info ('Successfully prendered page');
  }
  return true;
}

window['diagnoseError'] = function diagnoseError(debug, vtree, node) {
    if (debug) console.warn('VTree differed from node', vtree, node);
}

window['walk'] = function walk(debug, vtree, node, doc) {
  // This is slightly more complicated than one might expect since
  // browsers will collapse consecutive text nodes into a single text node.
  // There can thus be fewer DOM nodes than VDOM nodes.
  var vdomChild,
      domChild;

  vtree['domRef'] = node;

  // Fire onCreated events as though the elements had just been created.
  window['callCreated'](vtree);

  vtree.children = window['collapseSiblingTextNodes'](vtree.children);
  for (var i = 0; i < vtree.children.length; i++) {
    vdomChild = vtree['children'][i];
    domChild = node.childNodes[i];
      if (!domChild) {
	  window['diagnoseError'](debug,vdomChild, domChild);
	  return false;
      }
    if (vdomChild.type === 'vtext') {
        if (domChild.nodeType !== Node.TEXT_NODE) {
  	    window['diagnoseError'](debug, vdomChild, domChild);
	    return false;
	}

        if (vdomChild['text'] === domChild.textContent) {
          vdomChild['domRef'] = domChild;
        } else {
          window['diagnoseError'](debug, vdomChild, domChild);
          return false;
	}
    } else {
      if (domChild.nodeType !== Node.ELEMENT_NODE) return false;
      vdomChild['domRef'] = domChild;
      if(!window['walk'](debug, vdomChild, domChild, doc)) return false;
    }

  }
  // After walking the sizes of VDom and DOM should be equal
  // Otherwise there are DOM nodes unaccounted for
  if (vtree.children.length !== node.childNodes.length) {
     window['diagnoseError'](debug, vdomChild, domChild);
     return false;
  }
  return true;
}
