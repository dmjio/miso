function copyDOMIntoVTree (vtree) {
    walk (vtree, document.body.firstChild);
}

function walk (vtree, node) {
    var i = 0, vdomChild, domChild;
    vtree.domRef = node;
    while (i < vtree.children.length) {
      vdomChild = vtree.children[i];
      domChild = node.childNodes[i];
      if (vdomChild.type === "vtext") {
	  vdomChild.domRef = domChild;
	  i++;
	  continue;
      }
      walk(vdomChild, domChild);
      i++;
   }
}

