// Import the one runtime value (getDOMRef) from its leaf module and the rest as
// types, rather than through the `../../../miso` barrel: the barrel re-exports
// several `type`s as values, which the bun test runtime can't resolve (see
// ts/spec/native-mts.spec.ts). This also drops an unnecessary coupling.
import { getDOMRef } from '../../../miso/util';
import { VTreeType } from '../../../miso/types';
import type { VTree, DrawingContext, EventContext, EventCapture, EventKey, ProcessEvent } from '../../../miso/types';
import type { ElementRef } from '@lynx-js/type-element-api';

function buildStack(root: ElementRef, target: ElementRef, ctx: EventContext<ElementRef>): Array<number> {
  const stack: Array<number> = [];
  while (!ctx.isEqual(root, target)) {
    // Guard the config read like `nodeIdOf` does elsewhere: an element on the
    // target -> mount chain created outside the initial-draw/patch path has no
    // `nodeId` in its config, and an unguarded `.nodeId` would throw mid-route.
    const nid = (__GetConfig(target) as any)?.nodeId as number | undefined;
    if (nid !== undefined) stack.unshift(nid);
    const parent = ctx.parentNode(target);
    if (parent) {
      target = parent;
    } else {
      return stack;
    }
  }
  return stack;
}

function nextNodeId () : number {
  return globalThis['nodeId']++;
}

// --- <list> virtualization (minimal, non-recycling) ---------------------------
// Lynx <list> is a recycler: its children are NOT mounted directly. The engine
// drives rendering via a `componentAtIndex` callback, and it only starts asking
// for cells once we declare them through the `update-list-info` attribute. We
// hold each list's item elements and serve them back by index — one element per
// cell, with no reuse pool (that's the "full recycler" upgrade path).
type ListState = { node: ElementRef; items: Array<ElementRef>; known: number };
// Exported (read/write) for test introspection only -- production code should
// go through listStateOf/destroyListState, not touch this Map directly.
export const listStates = new Map<number, ListState>();

// Main-thread event routing registry, keyed by the element's `nodeId`.
// NOT stored in element config directly: Lynx __SetConfig/__GetConfig round-trips
// flat scalars (`nodeId`) but drops nested objects, so a nested registry written
// at paint read back `undefined` at dispatch. We also can't key by
// __GetElementUniqueID (not stable between the painted element and the tap
// target). `nodeId` is the identity the whole patch/event system uses and DOES
// round-trip via config — it's how buildStack ships the BTS delegation stack —
// so we read it back with `__GetConfig(node).nodeId` on both sides.
// Set in addEvent, read in routeEvent, cleared in removeEvent.
type MainThreadEntry = { staticKey : string; componentId : number; options : any };
type MainThreadKeys  = { captures : Record<string, MainThreadEntry>; bubbles : Record<string, MainThreadEntry> };
const mainThreadKeys = new Map<number, MainThreadKeys>();

// Direct-bind event names attached per node (via `__AddEvent` in `addEvent`),
// tracked by nodeId so they can be torn down when the node is destroyed. These
// element-level listeners are otherwise bound for the element's whole lifetime
// (the capability set is immutable for a reused node), so their only removal
// point is destruction — see `destroyNodeEvents`.
const directBindings = new Map<number, Set<string>>();

// Tear down all event state for a destroyed node. Called from `dropChildren`
// (ts/miso/native/mts.ts) when a subtree is removed, so neither the main-thread
// routing registry nor the native direct-bind listeners leak per unmounted node.
//
// Lynx has no standalone `__RemoveEvent`: `__AddEvent(node, type, name,
// undefined)` routes an empty callback to `FiberAddEvent`'s `RemoveEvent` path
// (see lynx/core/renderer/dom/fiber/fiber_element.cc), so that is the removal
// primitive for listeners bound with `__AddEvent`.
export function destroyNodeEvents(node : ElementRef, nodeId : number) : void {
  const direct = directBindings.get(nodeId);
  if (direct) {
    for (const name of direct) __AddEvent(node, 'catchEvent', name, undefined);
    directBindings.delete(nodeId);
  }
  mainThreadKeys.delete(nodeId);
}

// Drop a node's <list> virtualization state, if it has any. Keyed by
// __GetElementUniqueID (not nodeId, unlike destroyNodeEvents -- see
// listStateOf) so this must be called independently, not folded into
// destroyNodeEvents. Called from `dropChildren` (ts/miso/native/mts.ts) for
// every node in a removed subtree -- without this, a <list> removed via an
// ancestor wrapper (rather than directly) keeps its {node, items, known}
// entry, and every ElementRef in it, in this Map forever.
export function destroyListState(node : ElementRef) : void {
  listStates.delete(__GetElementUniqueID(node));
}

function nodeIdOf(node : ElementRef) : number | undefined {
  return (__GetConfig(node) as any)?.nodeId as number | undefined;
}

function listStateOf(parent: ElementRef): ListState | undefined {
  return listStates.get(__GetElementUniqueID(parent));
}

// Emit the incremental insert/remove diff (tail-based) so the engine learns how
// many cells exist and requests them via componentAtIndex.
function commitListInfo(st: ListState): void {
  const cur = st.items.length;
  if (cur === st.known) return;
  // Each insert action must carry the item's platform info — crucially the
  // `item-key` — inline in the payload, not just as an element attribute.
  // react-lynx builds `{ position, type, ...__listItemPlatformInfo }` (see
  // listUpdateInfo.ts `__toAttribute`), where the spread supplies item-key /
  // reuse-identifier. Omitting item-key here makes Lynx's "parse insert"
  // reject the cells ("illegal list item-key") and the list renders empty.
  // We read the info back off each stored <list-item> element.
  const insertAction: Array<Record<string, any>> = [];
  const removeAction: Array<{ position: number }> = [];
  if (cur > st.known) {
    for (let i = st.known; i < cur; i++) {
      const el = st.items[i];
      const itemKey = __GetAttributeByName(el, 'item-key');
      const reuseId = __GetAttributeByName(el, 'reuse-identifier');
      const action: Record<string, any> = { position: i, type: 'list-item', 'item-key': itemKey };
      if (reuseId != null) action['reuse-identifier'] = reuseId;
      insertAction.push(action);
    }
  } else {
    for (let i = st.known - 1; i >= cur; i--) removeAction.push({ position: i });
  }
  __SetAttribute(st.node, 'update-list-info', { insertAction, removeAction } as any);
  st.known = cur;
}

// Route one native event through main-thread delegation: fire any main-thread
// handlers along the target -> mount chain (phase-ordered), otherwise forward
// the delegation stack to the BTS which runs its own delegateEvent over the
// real VTree. Shared by the mount delegator (bubbling events) and the direct
// per-element bindings used for non-bubbling input events.
export function routeEvent(e: Event, name: string, capture: boolean, mount: ElementRef, ctx: EventContext<ElementRef>): void {
  const target = ctx.getTarget(e);
  const phase = capture ? 'captures' : 'bubbles';
  const chain : Array<ElementRef> = [];
  for (let node : ElementRef | null = target; node; node = ctx.parentNode(node)) {
    chain.push(node);
    if (ctx.isEqual(node, mount)) break;
  }
  // Capture runs mount -> target (top-down); bubble runs target -> mount
  // (bottom-up). chain is target-first, so reverse for capture.
  const order = capture ? chain.slice().reverse() : chain;
  const other = phase === 'bubbles' ? 'captures' : 'bubbles';
  let fired = false;
  for (const node of order) {
    const nid = nodeIdOf(node);
    const reg = nid !== undefined ? mainThreadKeys.get(nid) : undefined;
    // The registry encodes the handler's intended phase; whether Lynx delivered
    // the tap via its capture-catch or bubble-catch listener is an unrelated
    // implementation detail. Dispatch the handler whenever its element sits on
    // the propagation chain — prefer the fired phase, fall back to the other.
    const entry = reg ? (reg[phase]?.[name] ?? reg[other]?.[name]) : undefined;
    if (!entry) continue;
    fired = true;
    // Route in JS: apply the handler's options, then dispatch on the Haskell
    // layer (no BTS round-trip). stopPropagation halts the climb.
    if (entry.options.preventDefault && (e as any).preventDefault) (e as any).preventDefault();
    (globalThis['runtime'] as any)['dispatchMainThreadEvent']
      ({ componentId: entry.componentId, staticKey: entry.staticKey, event: e, target: node });
    if (entry.options.stopPropagation) break;
  }
  if (!fired) {
    // No main-thread handler on this chain — it's a background event.
    const jsContext = lynx.getJSContext();
    const stack = buildStack(mount, target, ctx);
    const msg : ProcessEvent = { event: e, stack, type: 'processEvent' };
    jsContext.dispatchEvent({ type: 'Miso.events', data: msg });
  }
}

export const eventContext : EventContext<ElementRef> = {
  delegator : (mount: ElementRef, events: Array<EventCapture>, _getVTree, _debug, ctx: EventContext<ElementRef>) => {
    for (const { name, capture } of events) {
      ctx.addEventListener(mount, name, (event: Event | Array<Event>) => {
        const evts = Array.isArray(event) ? event : [event];
        for (const e of evts) routeEvent(e, name, capture, mount, ctx);
      }, capture, null);
    }
  },
  addEventListener : (mount : ElementRef, event : string, listener, capture : boolean) => {
    const eventType = capture ? 'capture-catch' : 'catchEvent';
    return __AddEvent(mount, eventType, event, { type : 'worklet', value : listener });
  },
  isEqual : (x, y) => {
    return __ElementIsEqual(x,y);
  },
  getTarget : (e) => {
    /* BASE_STATIC_STRING_DECL(kElementRefptr, "elementRefptr"); */
    return (e.target as any).elementRefptr as ElementRef;
  },
  parentNode : (node: ElementRef) => {
    return __GetParent(node);
  }
};

/* Apply patches from BTS on MTS via PAPI calls */
export const drawingContext : DrawingContext<ElementRef> = {
  addClass : (className : string, domRef : ElementRef) => {
      __AddClass(domRef, className);
  },
  removeClass : (className : string, domRef : ElementRef) => {
      /* dmj: PR a __RemoveClass PAPI call to lynx ? */
      const classes = __GetClasses(domRef);
      if (classes.includes(className)) {
          const updated = classes.filter((x) => x !== className);
          __SetClasses(domRef, updated.join(' '));
      }
  },
  /* Update the node's eventKeys registry in place, keyed by phase; the delegator
     (above) reads it back via __GetConfig at dispatch time. Read-modify-write so
     we don't clobber nodeId or sibling event keys. */
  addEvent : (node : ElementRef, name : string, key : EventKey) => {
      // Direct-bind: attach a real element-level listener (Lynx native events
      // that don't bubble to the mount delegator). Feeds the same routeEvent.
      if (key.direct) {
          __AddEvent(node, 'catchEvent', name, { type : 'worklet', value : (event: Event | Array<Event>) => {
              const evts = Array.isArray(event) ? event : [event];
              for (const e of evts) routeEvent(e, name, false, globalThis['page'], eventContext);
          }});
          // Record the binding so it can be torn down on node destroy
          // (`destroyNodeEvents`); there is no per-render removal path.
          const directId = nodeIdOf(node);
          if (directId !== undefined) {
              const set = directBindings.get(directId) ?? new Set<string>();
              set.add(name);
              directBindings.set(directId, set);
          }
      }
      // Main-thread routing registry: only for handlers carrying a staticKey.
      // Keyed by the element's stable unique id in `mainThreadKeys` (see note at
      // its declaration) — NOT element config, which drops nested objects.
      if (key.staticKey === undefined) return;
      const nodeId = nodeIdOf(node);
      if (nodeId === undefined) {
        console.error('[miso mts] REG SKIPPED (no nodeId on node) name=' + name);
        return;
      }
      const reg = mainThreadKeys.get(nodeId) ?? { captures : {}, bubbles : {} };
      const phase = key.capture ? 'captures' : 'bubbles';
      reg[phase][name] = { staticKey : key.staticKey, componentId : key.componentId, options : key.options };
      mainThreadKeys.set(nodeId, reg);
  },
  removeEvent : (node : ElementRef, name : string, capture : boolean) => {
      const nodeId = nodeIdOf(node);
      const reg = nodeId !== undefined ? mainThreadKeys.get(nodeId) : undefined;
      if (!reg) return;
      const phase = capture ? 'captures' : 'bubbles';
      delete reg[phase][name];
  },
  nextSibling : (x : VTree<ElementRef>) => {
      let sibling = x.nextSibling;
      while (sibling) {
        switch (sibling.type) {
          case VTreeType.VComp:
          case VTreeType.VFrag: {
            const ref = getDOMRef(sibling);
            if (ref) return ref;
            sibling = sibling.nextSibling;
            break;
          }
          default:
            return sibling.domRef;
        }
      }
      return null;
  },
  createTextNode : (s: string) => {
    const node = __CreateRawText(s) as ElementRef;
    // Scope every element to the global stylesheet (cssId 0) so class rules
    // from styles.css resolve. Miso creates elements via raw element PAPI, and
    // those do NOT inherit the page's cssId — each one must be tagged
    // explicitly. See ts/miso/native/mts.ts `__SetCSSId([page],0)`.
    __SetCSSId ([node], 0);
    if (globalThis['initialDraw']) {
        const nodeId: number = nextNodeId ();
        globalThis['runtime']['nodes'][nodeId] = node;
        __SetConfig (node, { nodeId });
    }
    return node;
  },
  createElementNS : (_ns : string, tag : string) => {
    // Lynx has no XML namespaces; delegate to the tag-based factory, which also
    // handles cssId scoping and the initial-draw nodeId assignment. (The old
    // `globalThis.miso.context.createElement` path referenced a nonexistent
    // object — `globalThis.miso` only holds drawingContext/eventContext — and
    // threw for any namespaced element.)
    return drawingContext.createElement(tag);
  },
  createElement : (tag : string) => {
      var pageId = globalThis['native']['currentPageId'];
      var node = undefined;
      switch (tag) {
          case 'view':
              node = __CreateView(pageId);
              break;
          case 'scroll-view':
              node = __CreateScrollView(pageId);
              break;
          case 'text':
              node = __CreateText(pageId);
              break;
          case 'list': {
              node = __CreateList(
                  pageId,
                  (list, listID, cellIndex, opId) => {
                      const st = listStates.get(__GetElementUniqueID(list));
                      const root = st && st.items[cellIndex];
                      if (!root) return undefined;
                      __AppendElement(list, root);
                      const sign = __GetElementUniqueID(root);
                      __FlushElementTree(root, { triggerLayout: true, operationID: opId, elementID: sign, listID } as any);
                      return sign;
                  },
                  () => { /* non-recycling: each cell keeps its own element */ },
                  null,
              );
              listStates.set(__GetElementUniqueID(node), { node, items: [], known: 0 });
              break;
          }
          case 'image':
              node = __CreateImage(pageId);
              break;
          case 'frame':
              // options is optional (Record<string, unknown>); passing `null`
              // trips a native `NSArray insertObject:atIndex: object cannot be
              // nil`, so omit it entirely — matches __CreateView/__CreateImage.
              node = __CreateFrame(pageId);
              break;
          default:
              node = __CreateElement(tag, pageId);
              break;
      }
      // A native creator can return nil for an unsupported/unregistered element
      // (a tag with no working PAPI on this Lynx build). `__SetCSSId([nil], 0)`
      // then crashes natively with `NSArray insertObject:atIndex: object cannot
      // be nil`. Guard it: name the offending tag and fall back to an empty
      // <view> so the tree stays valid instead of taking down the whole app.
      if (!node) {
          console.error('[createElement]: native creator returned nil for tag "' + tag + '" — falling back to <view>');
          node = __CreateView(pageId);
      }
      // Scope to the global stylesheet (cssId 0) so `className` rules resolve;
      // raw-PAPI elements do not inherit the page's cssId. createElementNS
      // delegates here, so it is covered too.
      __SetCSSId ([node], 0);
      if (globalThis['initialDraw']) {
          const nodeId: number = nextNodeId ();
          globalThis['runtime']['nodes'][nodeId] = node;
          __SetConfig (node, { nodeId });
      }
      return node;
  },
  appendChild : (parent, child) => {
    const st = listStateOf(parent);
    if (st) { st.items.push(child); return child; }
    return __AppendElement (parent, child);
  },
  replaceChild : (parent, n, o) => {
    return __ReplaceElements (parent, [n], [o]);
  },
  removeChild : (parent, child) => {
    const st = listStateOf(parent);
    if (st) {
      const i = st.items.indexOf(child);
      if (i >= 0) st.items.splice(i, 1);
      return child;
    }
    listStates.delete(__GetElementUniqueID(child));
    return __RemoveElement (parent, child);
  },
  insertBefore : (parent, child, node) => {
    const st = listStateOf(parent);
    if (st) {
      const i = st.items.indexOf(node);
      if (i < 0) st.items.push(child); else st.items.splice(i, 0, child);
      return child;
    }
    return __InsertElementBefore (parent, child, node);
  },
  swapDOMRefs: (a: ElementRef, b: ElementRef, p: ElementRef): void => {
    return __SwapElement(a,b);
  },
  setAttribute : (node, key, value) => {
    if (key === 'id') return __SetID(node, value);
    return __SetAttribute(node,key,value);
  },
  removeAttribute : (node : ElementRef, key: string) => {
    // The engine's Element::SetAttribute only takes the removal branch when
    // the value is lepus-empty (null/undefined) — an empty string is a
    // regular string value and gets stored in updated_attr_map_ instead of
    // being removed from it.
    return __SetAttribute(node, key, null);
  },
  setAttributeNS : (node, ns, key, value) => {
    return __SetAttribute(node,key,value);
  },
  setTextContent : (node, text) => {
    return __SetAttribute(node,'text',text);
  },
  setInlineStyle : (cCss, nCss, node) => {
    if (cCss != nCss)
      return __SetInlineStyles(node, nCss)
  },
  flush : () => {
    for (const st of listStates.values()) commitListInfo(st);
    // The `initialDraw` latch is cleared ONCE by the runtime after the whole root
    // mount completes (Miso.Runtime.initComponent), NOT per-flush: while it is set,
    // createElement/createTextNode self-assign parity nodeIds into runtime.nodes,
    // and the initial draw flushes once per mounted component — a per-flush flip
    // tripped on the first nested child, leaving later nodes unregistered.
    return __FlushElementTree();
  },
  getRoot : () => {
     return globalThis['page'];
  },
  getHead : () => {
    /* dmj: todo implement */
    return null;
  }
};

