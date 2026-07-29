import { NodeId, getDOMRef, VComp, DrawingContext, EventContext, EventCapture, EventKey, ProcessEvent } from '../../../miso';
import type { ElementRef } from '@lynx-js/type-element-api';

function buildStack(root: ElementRef, target: ElementRef, ctx: EventContext<ElementRef>): Array<number> {
  const stack: Array<number> = [];
  while (!ctx.isEqual(root, target)) {
    stack.unshift(__GetConfig(target).nodeId as number);
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

// Route one native event through main-thread delegation: fire any main-thread
// handlers along the target -> mount chain (phase-ordered), otherwise forward
// the delegation stack to the BTS which runs its own delegateEvent over the
// real VTree. Shared by the mount delegator (bubbling events) and the direct
// per-element bindings used for non-bubbling input events.
function routeEvent(e: Event, name: string, capture: boolean, mount: ElementRef, ctx: EventContext<ElementRef>): void {
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
  let fired = false;
  for (const node of order) {
    const entry = (__GetConfig(node) as any)?.eventKeys?.[phase]?.[name];
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
      }
      // Main-thread routing registry: only for handlers carrying a staticKey.
      if (key.staticKey === undefined) return;
      const config = (__GetConfig(node) as any) ?? {};
      const eventKeys = { captures : {}, bubbles : {}, ...config.eventKeys };
      const phase = key.capture ? 'captures' : 'bubbles';
      eventKeys[phase] = { ...eventKeys[phase], [name] : { staticKey : key.staticKey, componentId : key.componentId, options : key.options } };
      __SetConfig(node, { ...config, eventKeys });
  },
  removeEvent : (node : ElementRef, name : string, capture : boolean) => {
      const config = (__GetConfig(node) as any) ?? {};
      const phase = capture ? 'captures' : 'bubbles';
      if (config.eventKeys && config.eventKeys[phase] && name in config.eventKeys[phase]) {
          const eventKeys = { ...config.eventKeys, [phase] : { ...config.eventKeys[phase] } };
          delete eventKeys[phase][name];
          __SetConfig(node, { ...config, eventKeys });
      }
  },
  nextSibling : (x : VComp<NodeId>) => {
      return getDOMRef(x.nextSibling);
  },
  createTextNode : (s: string) => {
    const node = __CreateRawText(s);
    if (globalThis['initialDraw']) {
        const nodeId: number = nextNodeId ();
        globalThis['runtime']['nodes'][nodeId] = node;
        __SetConfig (node, { nodeId });
    }
    return node;
  },
  createElementNS : (ns : string, tag : string) => {
    const node = globalThis['miso']['context']['createElement'](tag);
    if (globalThis['initialDraw']) {
        const nodeId: number = nextNodeId ();
        globalThis['runtime']['nodes'][nodeId] = node;
        __SetConfig (node, { nodeId });
    }
    return node;
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
          case 'list':
              node = __CreateList(pageId, undefined, null, null);
              break;
          case 'image':
              node = __CreateImage(pageId);
              break;
          case 'frame':
              node = __CreateFrame(pageId, null);
              break;
          default:
              node = __CreateElement(tag, pageId);
              break;
      }
      if (globalThis['initialDraw']) {
          const nodeId: number = nextNodeId ();
          globalThis['runtime']['nodes'][nodeId] = node;
          __SetConfig (node, { nodeId });
      }
      return node;
  },
  appendChild : (parent, child) => {
    return __AppendElement (parent, child);
  },
  replaceChild : (parent, n, o) => {
    return __ReplaceElements (parent, [n], [o]);
  },
  removeChild : (parent, child) => {
    return __RemoveElement (parent, child);
  },
  insertBefore : (parent, child, node) => {
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
    return __SetAttribute(node, key, '');
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
    if (globalThis['initialDraw']) {
      globalThis['initialDraw'] = false;
    }
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

