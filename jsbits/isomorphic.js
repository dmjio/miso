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

window['copyDOMIntoVTree'] = function copyDOMIntoVTree(logLevel,mountPoint, vtree, doc) {
  if (!doc) { doc = window.document; }
  var mountChildIdx = 0, node;
  // If script tags are rendered first in body, skip them.
  if (!mountPoint) {
    if (doc.body.childNodes.length > 0) {
      node = doc.body.firstChild;
    } else {
      node = doc.body.appendChild (doc.createElement('div'));
    }
  } else if (mountPoint.childNodes.length === 0) {
    node = mountPoint.appendChild (doc.createElement('div'));
  } else {
    while (mountPoint.childNodes[mountChildIdx] && (mountPoint.childNodes[mountChildIdx].nodeType === Node.TEXT_NODE || mountPoint.childNodes[mountChildIdx].localName === 'script')){
      mountChildIdx++;
    }
    if (!mountPoint.childNodes[mountChildIdx]) {
      node = doc.body.appendChild (doc.createElement('div'));
    } else {
      node = mountPoint.childNodes[mountChildIdx];
    }
  }

  if (!window['walk'](logLevel,vtree, node, doc)) {
    if (logLevel) {
      console.warn('Could not copy DOM into virtual DOM, falling back to diff');
    }
    // Remove all children before rebuilding DOM
    while (node.firstChild) node.removeChild(node.lastChild);
    vtree['domRef'] = node;
    window['populate'](null, vtree, doc);
    return false;
  }
  if (logLevel) {
    console.info ('Successfully prendered page');
  }
  return true;
}

window['diagnoseError'] = function diagnoseError(logLevel, vtree, node) {
  if (logLevel) console.warn('VTree differed from node', vtree, node);
}

window['walk'] = function walk(logLevel, vtree, node, doc) {
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
      window['diagnoseError'](logLevel,vdomChild, domChild);
      return false;
    }
    if (vdomChild.type === 'vtext') {
      if (domChild.nodeType !== Node.TEXT_NODE) {
        window['diagnoseError'](logLevel, vdomChild, domChild);
        return false;
      }

      if (vdomChild['text'] === domChild.textContent) {
        vdomChild['domRef'] = domChild;
      } else {
        window['diagnoseError'](logLevel, vdomChild, domChild);
        return false;
      }
    } else {
      if (domChild.nodeType !== Node.ELEMENT_NODE) return false;
      vdomChild['domRef'] = domChild;
      if(!window['walk'](logLevel, vdomChild, domChild, doc)) return false;
    }
  }
  return true;
}
