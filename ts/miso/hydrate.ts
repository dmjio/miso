import { callCreated } from './dom';
import { getLastDOMRef } from './util';
import { Mount, DrawingContext, HydrationContext, VTree, VText, DOMRef, VTreeType } from './types';

/* prerendering / hydration / isomorphic support */
function collapseSiblingTextNodes(vs: Array<VTree<DOMRef>>): Array<VTree<DOMRef>> {
  var ax = 0, adjusted = vs.length > 0 ? [vs[0]] : [];
  for (var ix = 1; ix < vs.length; ix++) {
    if (adjusted[ax].type === VTreeType.VText && vs[ix].type === VTreeType.VText) {
      (adjusted[ax] as VText<DOMRef>).text += (vs[ix] as VText<DOMRef>).text;
      continue;
    }
    adjusted[++ax] = vs[ix];
  }
  // Recursively collapse inside any VFrag children: SSR's foldMap concatenates
  // text nodes at every nesting level, so nested VFrags need the same treatment.
  for (const v of adjusted) {
    if (v.type === VTreeType.VFrag) {
      v.children = collapseSiblingTextNodes(v.children);
    }
  }
  return adjusted;
}

export function hydrate(logLevel: boolean, mountPoint: DOMRef | Text, vtree: VTree<DOMRef>, context: HydrationContext<DOMRef>, drawingContext: DrawingContext<DOMRef>): boolean {

  /* hydration mountPoint must be the root */
  if (!vtree || !mountPoint) return false;

  /* Don't hydrate on text mountPoint */
  if (mountPoint.nodeType === 3) return false;

  // begin walking the DOM, report the result
  if (!walk(logLevel, vtree, context.firstChild(mountPoint as DOMRef), context, drawingContext)) {
    // If we failed to prerender because the structures were different, fallback to drawing
      if (logLevel) {
        console.warn('[DEBUG_HYDRATE] Could not copy DOM into virtual DOM, falling back to diff');
      }
      while (context.firstChild(mountPoint as DOMRef))
        drawingContext.removeChild(mountPoint as DOMRef, context.lastChild(mountPoint as DOMRef));

     return false;
  } else {
    if (logLevel) {
      console.info('[DEBUG_HYDRATE] Successfully prerendered page');
    }
  }
  return true;
}

function diagnoseError(logLevel: boolean, vtree: VTree<DOMRef>, node: Node): void {
  if (logLevel) console.warn('[DEBUG_HYDRATE] VTree differed from node', vtree, node);
}

// Advance past all DOM nodes owned by `tree` and return the next sibling.
function nextAfter(tree: VTree<DOMRef>): Node {
  return (getLastDOMRef(tree) as unknown as Node).nextSibling as Node;
}

function walk(logLevel: boolean, vtree: VTree<DOMRef>, node: Node, context: HydrationContext<DOMRef>, drawingContext: DrawingContext<DOMRef>): boolean {
  // This is slightly more complicated than one might expect since
  // browsers will collapse consecutive text nodes into a single text node.
  // There can thus be fewer DOM nodes than VDOM nodes.
  // We handle this in collapseSiblingTextNodes
  switch (vtree.type) {
      case VTreeType.VComp:
       let mounted: Mount<DOMRef> = vtree.mount (node.parentNode as DOMRef);
       vtree.componentId = mounted.componentId;
       vtree.child = mounted.componentTree;
       mounted.componentTree.parent = vtree;
       if (!walk(logLevel, vtree.child, node, context, drawingContext)) {
          return false;
       }
       break;
    case VTreeType.VFrag:
      // A fragment maps to consecutive sibling DOM nodes, one per child.
      // Each child may occupy >1 DOM node (nested VFrag, VComp with VFrag root),
      // so advance via nextAfter rather than +1.
      // Collapse adjacent VText children: SSR (foldMap renderBuilder) concatenates
      // consecutive text into one string, which the browser parses as one text node.
      vtree.children = collapseSiblingTextNodes(vtree.children);
      for (const child of vtree.children) {
        if (!node) {
          diagnoseError(logLevel, child, null);
          return false;
        }
        if (!walk(logLevel, child, node, context, drawingContext)) return false;
        node = nextAfter(child);
      }
      break;
    case VTreeType.VText:
       if (node.nodeType !== 3 || vtree.text.trim() !== node.textContent.trim()) {
         diagnoseError(logLevel, vtree, node);
         return false;
       }
      vtree.domRef = node as DOMRef;
      break;
    case VTreeType.VNode:
      if (node.nodeType !== 1) {
         diagnoseError(logLevel, vtree, node);
         return false;
      }
      vtree.domRef = node as DOMRef;
      vtree.children = collapseSiblingTextNodes(vtree.children);
      // Fire onCreated events as though the elements had just been created.
      callCreated(node, vtree, drawingContext);

      // Use a DOM cursor rather than a numeric index: a VFrag child spans
      // multiple childNodes so the vtree index and DOM index diverge.
      let domCursor: Node = node.firstChild;
      for (var i = 0; i < vtree.children.length; i++) {
        const vdomChild = vtree.children[i];
        if (!domCursor) {
          diagnoseError(logLevel, vdomChild, null);
          return false;
        }
        if (!walk(logLevel, vdomChild, domCursor, context, drawingContext)) {
          return false;
        }
        domCursor = nextAfter(vdomChild);
      }
      break;
  }
  return true;
}
