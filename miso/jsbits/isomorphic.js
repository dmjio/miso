function copyDOMIntoVTree (vtree) {
    walk (vtree, document.body.firstChild);
}

function walk (vtree, node) {
    var i = 0;
    vtree.domRef = node;
    while (i < vtree.children.length) {
      walk(vtree.children[i], node.children[i]);
      i++;
   }
}

