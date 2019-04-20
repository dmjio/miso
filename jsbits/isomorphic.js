window.MisoException = function MisoException(message) {
  this.message = message;
  this.name = 'MisoException';
}

window.copyDOMIntoVTree = function copyDOMIntoVTree(vtree, document = window.document) {
  walk(vtree, document.body.firstChild, document);
}

window.walk = function walk(vtree, node, document) {
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
        if (domChild.nodeType !== Node.TEXT_NODE) {
          throw new MisoException("Expected text node but found got element");
        }

        if (vdomChild.text === domChild.textContent) {
          vdomChild.domRef = domChild;
        } else {
          var len = vdomChild.text.length,
              domNodeText = domChild.textContent.substring(0, len);
          if (domNodeText !== vdomChild.text) {
            throw new MisoException("Text in DOM does not match text in virtual DOM");
          }
          // There are more VDOM nodes than DOM nodes
          // Create new DOM node to ensure synchrony between VDom and DOM
          var partialTxt = document.createTextNode(domNodeText);
          node.insertBefore(partialTxt, domChild);
          vdomChild.domRef = partialTxt;
          domChild.textContent = domChild.textContent.substring(len);
        }
    } else {
      if (domChild.nodeType !== Node.ELEMENT_NODE) {
        throw new MisoException("Expected element but got text node");
      }
      walk(vdomChild, domChild, document);
    }

  }
  // After walking the sizes of VDom and DOM should be equal
  // Otherwise there are DOM nodes unaccounted for
  if (vtree.children.length !== node.childNodes.length) {
    throw new MisoException("Non-matching DOM an VDOM");
  }
}
