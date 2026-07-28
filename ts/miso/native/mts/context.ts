import { NodeId, getDOMRef, VComp, DrawingContext, EventContext, EventCapture, ProcessEvent } from '../../../miso';
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

export const eventContext : EventContext<ElementRef> = {
  delegator : (mount: ElementRef, events: Array<EventCapture>, _getVTree, _debug, ctx: EventContext<ElementRef>) => {
    for (const { name, capture } of events) {
      ctx.addEventListener(mount, name, (event: Event | Array<Event>) => {
        const evts = Array.isArray(event) ? event : [event];
        for (const e of evts) {
          const target = ctx.getTarget(e);
          // Delegate: walk target -> mount looking for a handler whose config
          // carries an eventKey ("componentId:staticKey") for this event. Only
          // 'mainThread'-marked handlers carry one (see onWithOptions), so its
          // presence means "dispatch on the main thread here". If none is found,
          // it's a background handler — forward the delegation stack to the BTS.
          let node : ElementRef | null = target;
          let combined : string | undefined = undefined;
          while (node) {
            const eventKeys = (__GetConfig(node) as any)?.eventKeys;
            if (eventKeys && eventKeys[name]) { combined = eventKeys[name]; break; }
            if (ctx.isEqual(node, mount)) break;
            node = ctx.parentNode(node);
          }
          if (combined) {
            // Main-thread dispatch on the Haskell layer, no BTS round-trip.
            const idx = combined.indexOf(':');
            const componentId = Number(combined.slice(0, idx));
            const staticKey = combined.slice(idx + 1);
            (globalThis['runtime'] as any)['dispatchMainThreadEvent']
              ({ componentId, staticKey, event: e, target: node });
          } else {
            // Background-thread event: build the delegation stack and forward.
            const jsContext = lynx.getJSContext();
            const stack = buildStack(mount, target, ctx);
            const msg : ProcessEvent = { event: e, stack, type: 'processEvent' };
            jsContext.dispatchEvent({ type: 'Miso.events', data: msg });
          }
        }
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
  createElementNS : (ns : string, tag : string, events?: Record<string,string>) => {
    const node = globalThis['miso']['context']['createElement'](tag);
    if (globalThis['initialDraw']) {
        const nodeId: number = nextNodeId ();
        globalThis['runtime']['nodes'][nodeId] = node;
        __SetConfig (node, events ? { nodeId, eventKeys: events } : { nodeId });
    }
    return node;
  },
  createElement : (tag : string, events?: Record<string,string>) => {
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
          __SetConfig (node, events ? { nodeId, eventKeys: events } : { nodeId });
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

