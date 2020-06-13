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

window['copyDOMIntoVTree'] = function copyDOMIntoVTree(mountPoint, vtree, doc) {
  if (!doc) { doc = window.document; }
  var node = mountPoint ? mountPoint.firstChild : doc.body.firstChild;
  if (!window['walk'](vtree, node, doc)) {
    console.warn('Could not copy DOM into virtual DOM, falling back to diff');
    // Remove all children before rebuilding DOM
    window['diff'](null, vtree, node.parentNode, doc);
    return false;
  }
  return true;
}

window['walk'] = function walk(vtree, node, doc) {
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
    if (!domChild) return false;
    if (vdomChild.type === 'vtext') {
        if (domChild.nodeType !== Node.TEXT_NODE) return false;

        if (vdomChild['text'] === domChild.textContent) {
          vdomChild['domRef'] = domChild;
        } else {
          return false;
	}
    } else {
      if (domChild.nodeType !== Node.ELEMENT_NODE) return false;
      vdomChild['domRef'] = domChild;
      window['walk'](vdomChild, domChild, doc);
    }

  }
  // After walking the sizes of VDom and DOM should be equal
  // Otherwise there are DOM nodes unaccounted for
  if (vtree.children.length !== node.childNodes.length) return false;

  return true;
}
