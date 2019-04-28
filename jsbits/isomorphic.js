window.copyDOMIntoVTree = function copyDOMIntoVTree(vtree, doc = window.document) {
  var node = doc.body.firstChild;
  if (!walk(vtree, node, doc)) {
    console.warn('Could not copy DOM into virtual DOM, falling back to diff');
    // Remove all children before rebuilding DOM
    while (node.firstChild)
      node.removeChild(node.lastChild);
    window.diff(null, vtree, doc.body.firstChild, doc);
    return false;
  }
  return true;
}

window.walk = function walk(vtree, node, doc) {
  // This is slightly more complicated than one might expect since
  // browsers will collapse consecutive text nodes into a single text node.
  // There can thus be fewer DOM nodes than VDOM nodes.
  var vdomChild,
      domChild;

  vtree.domRef = node;

  // Fire onCreated events as though the elements had just been created.
  callCreated(vtree);

  for (var i = 0; i < vtree.children.length; i++) {
    vdomChild = vtree.children[i];
    domChild = node.childNodes[i];
    if (vdomChild.type === "vtext") {
        if (domChild.nodeType !== Node.TEXT_NODE) return false;

        if (vdomChild.text === domChild.textContent) {
          vdomChild.domRef = domChild;
        } else {
          var len = vdomChild.text.length,
              domNodeText = domChild.textContent.substring(0, len);
          if (domNodeText !== vdomChild.text) return false;

          // There are more VDOM nodes than DOM nodes
          // Create new DOM node to ensure synchrony between VDom and DOM
          var partialTxt = doc.createTextNode(domNodeText);
          node.insertBefore(partialTxt, domChild);
          vdomChild.domRef = partialTxt;
          domChild.textContent = domChild.textContent.substring(len);
        }
    } else {
      if (domChild.nodeType !== Node.ELEMENT_NODE) return false;
      walk(vdomChild, domChild, doc);
    }

  }
  // After walking the sizes of VDom and DOM should be equal
  // Otherwise there are DOM nodes unaccounted for
  if (vtree.children.length !== node.childNodes.length) return false;

  return true;
}
