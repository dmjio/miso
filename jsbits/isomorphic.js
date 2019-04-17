window.MisoException = function MisoException(message) {
  this.message = message;
  this.name = 'MisoException';
}

window.copyDOMIntoVTree = function copyDOMIntoVTree(vtree, document = document) {
  walk(vtree, document.body.firstChild);
}

function walk(vtree, node) {
  // This is slightly more complicated than one might expect since
  // browsers will collapse consecutive text nodes into a single text node.
  // There can thus be fewer DOM nodes than VDOM nodes.
  var i = 0,
    j = 0,
    // This indicates if we are in a block of consecutive text nodes.
    inTextBlock = false,
    vdomChild,
    domChild;
  vtree.domRef = node;

  // Fire onCreated events as though the elements had just been created.
  callCreated(vtree);

  for (i = 0; i < vtree.children.length; i++) {
    vdomChild = vtree.children[i];
    domChild = node.childNodes[j];
    if (vdomChild.type === "vtext") {
      if (!inTextBlock) {
        // This is the first text node in a block of consecutive text nodes.
        inTextBlock = true;
        if (domChild.nodeType !== Node.TEXT_NODE) {
          throw new MisoException("Expected text node but found got element");
        }
        // Since the text nodes are collapsed on the DOM side, the text
        // contents can be different here. To make sure the VDOM and
        // the DOM agree we copy the text contents from the DOM to the VDOM.
        vdomChild.text = domChild.textContent;
        vdomChild.domRef = domChild;
        j++;
      } else {
        // In this case there have been other text nodes directly before this node.
        // There is thus no corresponding DOM node and we need to create a new node.
        vdomChild.domRef = document.createTextNode("");
        vdomChild.text = "";
      }
    } else {
      inTextBlock = false;
      if (domChild.nodeType !== Node.ELEMENT_NODE) {
        throw new MisoException("Expected element but got text node");
      }
      walk(vdomChild, domChild);
      j++;
    }
  }
}
