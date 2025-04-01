import { callCreated, populate } from './dom';
import { VTree } from './types';

/* prerendering / hydration / isomorphic support */
function collapseSiblingTextNodes(vs: Array<VTree>): Array<VTree> {
  var ax = 0,
    adjusted = vs.length > 0 ? [vs[0]] : [];
  for (var ix = 1; ix < vs.length; ix++) {
    if (adjusted[ax]['type'] === 'vtext' && vs[ix]['type'] === 'vtext') {
      adjusted[ax]['text'] += vs[ix]['text'];
      continue;
    }
    adjusted[++ax] = vs[ix];
  }
  return adjusted;
}

export function hydrate(logLevel: boolean, mountPoint: Element | Text, vtree: VTree): boolean {
  var mountChildIdx = 0,
    node;
  // If script tags are rendered first in body, skip them.
  if (!mountPoint) {
    if (document.body.childNodes.length > 0) {
      node = document.body.firstChild;
    } else {
      node = document.body.appendChild(document.createElement('div'));
    }
  } else if (mountPoint.childNodes.length === 0) {
    node = mountPoint.appendChild(document.createElement('div'));
  } else {
    while (
      mountPoint.childNodes[mountChildIdx] &&
      (mountPoint.childNodes[mountChildIdx].nodeType === 3 ||
        (mountPoint.childNodes[mountChildIdx] as Element).localName === 'script')
    ) {
      mountChildIdx++;
    }
    if (!mountPoint.childNodes[mountChildIdx]) {
      node = document.body.appendChild(document.createElement('div'));
    } else {
      node = mountPoint.childNodes[mountChildIdx];
    }
  }
  if (!walk(logLevel, vtree, node)) {
    if (logLevel) {
      console.warn('Could not copy DOM into virtual DOM, falling back to diff');
    }
    // Remove all children before rebuilding DOM
    while (node.firstChild) node.removeChild(node.lastChild);
    vtree['domRef'] = node;
    populate(null, vtree);
    return false;
  } else {
    if (logLevel) {
      if (!integrityCheck(vtree)) {
        console.warn('Integrity check completed with errors');
      } else {
        console.info('Successfully prerendered page');
      }
    }
  }
  return true;
}
function diagnoseError(logLevel: boolean, vtree: VTree, node: Element): void {
  if (logLevel) console.warn('VTree differed from node', vtree, node);
}
// https://stackoverflow.com/questions/11068240/what-is-the-most-efficient-way-to-parse-a-css-color-in-javascript
function parseColor(input: string): number[] {
  if (input.substr(0, 1) == '#') {
    var collen = (input.length - 1) / 3;
    var fact = [17, 1, 0.062272][collen - 1];
    return [
      Math.round(parseInt(input.substr(1, collen), 16) * fact),
      Math.round(parseInt(input.substr(1 + collen, collen), 16) * fact),
      Math.round(parseInt(input.substr(1 + 2 * collen, collen), 16) * fact),
    ];
  } else
    return input
      .split('(')[1]
      .split(')')[0]
      .split(',')
      .map((x: string) => {
        return +x;
      });
}
export function integrityCheck(vtree: VTree): boolean {
  return check(true, vtree);
}

// dmj: Does deep equivalence check, spine and leaves of virtual DOM to DOM.
function check(result: boolean, vtree: VTree): boolean {
  // text nodes must be the same
  if (vtree['type'] == 'vtext') {
    if (vtree['domRef'].nodeType !== 3) {
      console.warn('VText domRef not a TEXT_NODE', vtree);
      result = false;
    } else if (vtree['text'] !== vtree['domRef'].textContent) {
      console.warn('VText node content differs', vtree);
      result = false;
    }
  } // if vnode / vcomp, must be the same
  else {
    // tags must be identical
    if (vtree['tag'].toUpperCase() !== vtree['domRef'].tagName) {
      console.warn(
        'Integrity check failed, tags differ',
        vtree['tag'].toUpperCase(),
        vtree['domRef'].tagName,
      );
      result = false;
    }
    // Child lengths must be identical
    if ('children' in vtree && vtree['children'].length !== vtree['domRef'].childNodes.length) {
      console.warn(
        'Integrity check failed, children lengths differ',
        vtree,
        vtree.children,
        vtree['domRef'].childNodes,
      );
      result = false;
    }
    // properties must be identical
    var keyLength = Object.keys(vtree['props']).length,
      key = null;
    for (var i = 0; i < keyLength; i++) {
      key = Object.keys(vtree['props'])[i];
      if (key === 'href') {
        var absolute = window.location.origin + '/' + vtree['props'][key],
          url = vtree['domRef'][key],
          relative = vtree['props'][key];
        if (
          absolute !== url &&
          relative !== url &&
          relative + '/' !== url &&
          absolute + '/' !== url
        ) {
          console.warn('Property ' + key + ' differs', vtree['props'][key], vtree['domRef'][key]);
          result = false;
        }
      } else if (key === 'height' || key === 'width') {
        if (parseFloat(vtree['props'][key]) !== parseFloat(vtree['domRef'][key])) {
          console.warn('Property ' + key + ' differs', vtree['props'][key], vtree['domRef'][key]);
          result = false;
        }
      } else if (key === 'class' || key === 'className') {
        if (vtree['props'][key] !== vtree['domRef'].className) {
          console.warn('Property class differs', vtree['props'][key], vtree['domRef'].className);
          result = false;
        }
      } else if (!vtree['domRef'][key]) {
        if (vtree['props'][key] !== vtree['domRef'].getAttribute(key)) {
          console.warn(
            'Property ' + key + ' differs',
            vtree['props'][key],
            vtree['domRef'].getAttribute(key),
          );
          result = false;
        }
      } else if (vtree['props'][key] !== vtree['domRef'][key]) {
        console.warn('Property ' + key + ' differs', vtree['props'][key], vtree['domRef'][key]);
        result = false;
      }
    }
    // styles must be identical
    keyLength = Object.keys(vtree['css']).length;
    for (i = 0; i < keyLength; i++) {
      key = Object.keys(vtree['css'])[i];
      if (key === 'color') {
        if (
          parseColor(vtree['domRef'].style[key]).toString() !==
          parseColor(vtree['css'][key]).toString()
        ) {
          console.warn('Style ' + key + ' differs', vtree['css'][key], vtree['domRef'].style[key]);
          result = false;
        }
      } else if (vtree['css'][key] !== vtree['domRef'].style[key]) {
        console.warn('Style ' + key + ' differs', vtree['css'][key], vtree['domRef'].style[key]);
        result = false;
      }
    }
    // recursive call for `vnode` / `vcomp`
    for (const child of vtree['children']) result &&= check(result, child);
  }
  return result;
}

function walk(logLevel: boolean, vtree: VTree, node: Element): boolean {
  // This is slightly more complicated than one might expect since
  // browsers will collapse consecutive text nodes into a single text node.
  // There can thus be fewer DOM nodes than VDOM nodes.
  // We handle this in collapseSiblingTextNodes
  var vdomChild: VTree, domChild: Element;
  vtree['domRef'] = node;
  // Fire onCreated events as though the elements had just been created.
  callCreated(vtree);
  vtree.children = collapseSiblingTextNodes(vtree.children);
  for (var i = 0; i < vtree.children.length; i++) {
    vdomChild = vtree['children'][i];
    domChild = node.childNodes[i] as Element;
    if (!domChild) {
      diagnoseError(logLevel, vdomChild, domChild);
      return false;
    }
    if (vdomChild.type === 'vtext') {
      if (domChild.nodeType !== 3) {
        diagnoseError(logLevel, vdomChild, domChild);
        return false;
      }
      if (vdomChild['text'] === domChild.textContent) {
        vdomChild['domRef'] = domChild;
      } else {
        diagnoseError(logLevel, vdomChild, domChild);
        return false;
      }
    } else if (vdomChild['type'] === 'vcomp') {
      vdomChild['mount']((component: VTree) => {
        vdomChild['children'].push(component);
        walk(logLevel, vdomChild, domChild);
      });
    } else {
      if (domChild.nodeType !== 1) return false;
      vdomChild['domRef'] = domChild;
      walk(logLevel, vdomChild, domChild);
    }
  }
  return true;
}
