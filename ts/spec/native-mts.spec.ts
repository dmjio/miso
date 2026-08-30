/* Smoke tests for the MTS dual-thread event layer (ts/miso/native/mts/context.ts).
   These exercise the riskiest, previously-untested native logic: main-thread
   event routing (phase order + stopPropagation), the background-thread fallback,
   and per-node event-registry teardown. The Lynx PAPI globals (`__GetConfig`,
   `__AddEvent`) and the `runtime`/`lynx` host objects are stubbed here — in a web
   build they don't exist, which is exactly why this code has no other coverage. */
import { test, expect, describe, beforeEach, afterEach, afterAll, beforeAll } from 'bun:test';
import { routeEvent, destroyNodeEvents, drawingContext, listStates, destroyListState } from '../miso/native/mts/context';
import { dropChildren } from '../miso/native/mts';
import { vnode, vfrag, vcomp, vtext } from '../miso/smart';
import type { EventContext, NodeId } from '../miso/types';

/* silence the module's console.error diagnostics */
beforeAll(() => { console.error = () => {}; console.log = () => {}; });

type El = { nodeId: number };

/* --- stubbed Lynx PAPI + host globals ---------------------------------------- */
let addEventCalls: Array<any[]>;
let mtsDispatches: Array<any>;
let btsDispatches: Array<any>;

beforeEach(() => {
  addEventCalls = [];
  mtsDispatches = [];
  btsDispatches = [];
  // Elements are plain { nodeId } objects, so config *is* the element.
  (globalThis as any).__GetConfig = (n: El) => n;
  (globalThis as any).__AddEvent = (...args: any[]) => addEventCalls.push(args);
  (globalThis as any).page = { nodeId: -1 };
  (globalThis as any).runtime = {
    dispatchMainThreadEvent: (arg: any) => mtsDispatches.push(arg),
  };
  (globalThis as any).lynx = {
    getJSContext: () => ({ dispatchEvent: (x: any) => btsDispatches.push(x) }),
  };
});

afterAll(() => {
  for (const g of ['__GetConfig', '__AddEvent', 'page', 'runtime', 'lynx'])
    delete (globalThis as any)[g];
});

/* A fake EventContext over an explicit parent map, so routeEvent can walk the
   target -> mount chain without any real element PAPI. */
function makeCtx(target: El, parentOf: Map<number, El | null>): EventContext<El> {
  return {
    isEqual: (a: El, b: El) => a.nodeId === b.nodeId,
    parentNode: (n: El) => parentOf.get(n.nodeId) ?? null,
    getTarget: (_e: Event) => target,
    // unused by routeEvent:
    delegator: () => {},
    addEventListener: () => undefined,
  } as unknown as EventContext<El>;
}

const opts = (o: Partial<{ preventDefault: boolean; stopPropagation: boolean }> = {}) =>
  ({ preventDefault: false, stopPropagation: false, ...o });

describe('routeEvent — main-thread dispatch', () => {

  test('fires the handler on an element in the bubble chain', () => {
    // chain: target(2) -> mid(1) -> mount(0); handler registered on mid.
    const mount: El = { nodeId: 0 }, mid: El = { nodeId: 1 }, target: El = { nodeId: 2 };
    const ctx = makeCtx(target, new Map([[2, mid], [1, mount], [0, null]]));
    drawingContext.addEvent(mid, 'tap',
      { capture: false, staticKey: 'sk-mid', componentId: 42, options: opts() } as any);

    routeEvent({} as Event, 'tap', false, mount, ctx);

    expect(mtsDispatches.length).toBe(1);
    expect(mtsDispatches[0].componentId).toBe(42);
    expect(mtsDispatches[0].staticKey).toBe('sk-mid');
    expect(mtsDispatches[0].target).toBe(mid);
    expect(btsDispatches.length).toBe(0); // handled on MTS, no BTS round-trip
  });

  test('stopPropagation halts the climb (outer handler does not fire)', () => {
    const mount: El = { nodeId: 10 }, mid: El = { nodeId: 11 }, target: El = { nodeId: 12 };
    const ctx = makeCtx(target, new Map([[12, mid], [11, mount], [10, null]]));
    // mid stops propagation; mount also has a handler that must NOT fire.
    drawingContext.addEvent(mid, 'tap',
      { capture: false, staticKey: 'sk-mid', componentId: 1, options: opts({ stopPropagation: true }) } as any);
    drawingContext.addEvent(mount, 'tap',
      { capture: false, staticKey: 'sk-mount', componentId: 2, options: opts() } as any);

    routeEvent({} as Event, 'tap', false, mount, ctx);

    expect(mtsDispatches.length).toBe(1);
    expect(mtsDispatches[0].staticKey).toBe('sk-mid');
  });

  test('no main-thread handler falls back to the BTS with the delegation stack', () => {
    // Fresh nodeIds with nothing registered -> background event.
    const mount: El = { nodeId: 20 }, mid: El = { nodeId: 21 }, target: El = { nodeId: 22 };
    const ctx = makeCtx(target, new Map([[22, mid], [21, mount], [20, null]]));

    routeEvent({} as Event, 'tap', false, mount, ctx);

    expect(mtsDispatches.length).toBe(0);
    expect(btsDispatches.length).toBe(1);
    expect(btsDispatches[0].type).toBe('Miso.events');
    expect(btsDispatches[0].data.type).toBe('processEvent');
    // buildStack walks mid then mount-exclusive: [mid, target] by nodeId.
    expect(btsDispatches[0].data.stack).toEqual([21, 22]);
  });

  test('buildStack tolerates a chain element with no nodeId config', () => {
    // An element created outside the initial-draw/patch path has no config.
    const mount: El = { nodeId: 30 }, target: El = { nodeId: 32 };
    const orphan = {} as El; // __GetConfig(orphan)?.nodeId === undefined
    const ctx = makeCtx(target, new Map<number, El | null>([[32, orphan], [30, null]]));
    (ctx as any).parentNode = (n: El) =>
      n.nodeId === 32 ? orphan : (n === orphan ? mount : null);

    // Must not throw despite the missing config, and must drop the orphan id.
    expect(() => routeEvent({} as Event, 'tap', false, mount, ctx)).not.toThrow();
    expect(btsDispatches[0].data.stack).toEqual([32]);
  });
});

describe('destroyNodeEvents — registry teardown', () => {

  test('unbinds direct-bind listeners and is idempotent', () => {
    const node: El = { nodeId: 40 };
    drawingContext.addEvent(node, 'input', { capture: false, direct: true } as any);

    // one bind at registration
    const binds = addEventCalls.filter(a => a[2] === 'input');
    expect(binds.length).toBe(1);
    expect(binds[0][3]).not.toBeUndefined(); // a real worklet listener

    destroyNodeEvents(node, 40);
    const afterDestroy = addEventCalls.filter(a => a[2] === 'input');
    // teardown adds the __AddEvent(..., undefined) "RemoveEvent" call
    expect(afterDestroy.length).toBe(2);
    expect(afterDestroy[1][3]).toBeUndefined();

    // second destroy is a no-op (bindings already cleared)
    destroyNodeEvents(node, 40);
    expect(addEventCalls.filter(a => a[2] === 'input').length).toBe(2);
  });

  test('drops a node from main-thread routing so its handler no longer fires', () => {
    const mount: El = { nodeId: 50 }, target: El = { nodeId: 51 };
    const ctx = makeCtx(target, new Map([[51, mount], [50, null]]));
    drawingContext.addEvent(target, 'tap',
      { capture: false, staticKey: 'sk', componentId: 7, options: opts() } as any);

    destroyNodeEvents(target, 51);
    routeEvent({} as Event, 'tap', false, mount, ctx);

    expect(mtsDispatches.length).toBe(0);     // registry entry gone
    expect(btsDispatches.length).toBe(1);     // now treated as a background event
  });
});

describe('drawingContext.nextSibling — MTS', () => {

  test('returns the domRef of a plain VNode sibling', () => {
    const sibling = vnode<NodeId>({ tag: 'view', domRef: { nodeId: 1 } });
    const node = vnode<NodeId>({ tag: 'view', nextSibling: sibling });
    expect(drawingContext.nextSibling(node)).toBe(sibling.domRef);
  });

  test('drills into a VComp sibling to find its child domRef', () => {
    const inner = vnode<NodeId>({ tag: 'text', domRef: { nodeId: 2 } });
    const sibling = vcomp<NodeId>({});
    sibling.child = inner;
    const node = vnode<NodeId>({ tag: 'view', nextSibling: sibling });
    expect(drawingContext.nextSibling(node)).toBe(inner.domRef);
  });

  test('walks past an empty VFrag sibling to the one after it', () => {
    // node -> emptyFrag -> real. An empty VFrag renders nothing, so the
    // search must continue past it to the next sibling.
    const real = vnode<NodeId>({ tag: 'view', domRef: { nodeId: 3 } });
    const emptyFrag = vfrag<NodeId>([]);
    emptyFrag.nextSibling = real;
    const node = vnode<NodeId>({ tag: 'view', nextSibling: emptyFrag });
    expect(drawingContext.nextSibling(node)).toBe(real.domRef);
  });

  test('walks past an empty VComp (unmounted child) sibling to the one after it', () => {
    const real = vnode<NodeId>({ tag: 'view', domRef: { nodeId: 4 } });
    const emptyComp = vcomp<NodeId>({});
    emptyComp.child = null as any;
    emptyComp.nextSibling = real;
    const node = vnode<NodeId>({ tag: 'view', nextSibling: emptyComp });
    expect(drawingContext.nextSibling(node)).toBe(real.domRef);
  });

  test('returns null when every remaining sibling is empty', () => {
    const emptyFrag1 = vfrag<NodeId>([]);
    const emptyFrag2 = vfrag<NodeId>([]);
    emptyFrag1.nextSibling = emptyFrag2;
    const node = vnode<NodeId>({ tag: 'view', nextSibling: emptyFrag1 });
    expect(drawingContext.nextSibling(node)).toBeNull();
  });

  test('returns null with no next sibling at all', () => {
    const node = vnode<NodeId>({ tag: 'view' });
    expect(drawingContext.nextSibling(node)).toBeNull();
  });

  test('a VNode sibling with a null domRef halts the search — it is NOT skipped like an empty fragment', () => {
    // node -> notYetCreated(VNode, domRef: null) -> real. Unlike a VFrag/VComp,
    // a VNode/VText is not something the search walks past: dom.ts's reference
    // implementation returns the sibling's domRef directly in this case
    // (null), rather than continuing on to `real`.
    const real = vnode<NodeId>({ tag: 'view', domRef: { nodeId: 5 } });
    const notYetCreated = vnode<NodeId>({ tag: 'view', domRef: null });
    notYetCreated.nextSibling = real;
    const node = vnode<NodeId>({ tag: 'view', nextSibling: notYetCreated });
    expect(drawingContext.nextSibling(node)).toBeNull();
  });

  test('a VText sibling with a null domRef halts the search the same way', () => {
    const real = vnode<NodeId>({ tag: 'view', domRef: { nodeId: 6 } });
    const notYetCreated = vtext<NodeId>('hi'); // domRef: null by construction
    notYetCreated.nextSibling = real;
    const node = vnode<NodeId>({ tag: 'view', nextSibling: notYetCreated });
    expect(drawingContext.nextSibling(node)).toBeNull();
  });
});

describe('drawingContext.removeAttribute — MTS', () => {

  test('passes null, not an empty string, so the engine takes the removal branch', () => {
    // Element::SetAttribute (lynx/core/renderer/dom/element.cc) only removes
    // an attribute when the value is lepus-empty (null/undefined); an empty
    // string is a normal string value and gets stored instead of removed.
    let setAttributeCalls: Array<any[]> = [];
    (globalThis as any).__SetAttribute = (...args: any[]) => { setAttributeCalls.push(args); };

    const node: El = { nodeId: 300 };
    drawingContext.removeAttribute(node as any, 'disabled');

    expect(setAttributeCalls.length).toBe(1);
    expect(setAttributeCalls[0][1]).toBe('disabled');
    expect(setAttributeCalls[0][2]).toBeNull();

    delete (globalThis as any).__SetAttribute;
  });
});

// Regression: dropChildren (ts/miso/native/mts.ts) recursively tears down a
// removed subtree's event state via destroyNodeEvents, but previously never
// touched listStates (ts/miso/native/mts/context.ts) -- so a <list> removed
// via an ancestor wrapper (rather than directly) kept its {node, items,
// known} entry, and every ElementRef in it, in that Map forever.
describe('destroyListState / dropChildren — <list> virtualization teardown', () => {

  afterEach(() => {
    for (const g of ['__GetElementUniqueID', '__FirstElement', '__NextElement'])
      delete (globalThis as any)[g];
  });

  test('destroyListState clears a node\'s own list state', () => {
    const node: El = { nodeId: 72 };
    (globalThis as any).__GetElementUniqueID = (n: El) => n.nodeId;

    listStates.set(72, { node: node as any, items: [{} as any], known: 1 });
    expect(listStates.has(72)).toBeTrue();

    destroyListState(node as any);
    expect(listStates.has(72)).toBeFalse();
  });

  test('destroyListState on a node with no list state is a harmless no-op', () => {
    const node: El = { nodeId: 73 };
    (globalThis as any).__GetElementUniqueID = (n: El) => n.nodeId;
    expect(() => destroyListState(node as any)).not.toThrow();
  });

  test('dropChildren clears the listStates entry of a <list> nested under a removed wrapper', () => {
    // tree: wrapper(70) -> list(71), removed as a unit via the wrapper --
    // the list itself is never the direct argument to removeChild/replaceChild.
    const wrapper: El = { nodeId: 70 };
    const list: El = { nodeId: 71 };

    (globalThis as any).__GetElementUniqueID = (n: El) => n.nodeId;
    const childrenOf = new Map<number, El[]>([[70, [list]], [71, []]]);
    (globalThis as any).__FirstElement = (n: El) => childrenOf.get(n.nodeId)?.[0] ?? null;
    (globalThis as any).__NextElement = (n: El) => {
      for (const kids of childrenOf.values()) {
        const i = kids.indexOf(n);
        if (i >= 0 && i + 1 < kids.length) return kids[i + 1];
      }
      return null;
    };

    listStates.set(71, { node: list as any, items: [{} as any, {} as any], known: 2 });
    expect(listStates.has(71)).toBeTrue();

    const nodeMap: Record<number, El> = { 70: wrapper, 71: list };
    dropChildren(nodeMap as any, wrapper as any);

    // the list's virtualization state (and every ElementRef it held) is gone,
    // not just its runtime.nodes entry
    expect(listStates.has(71)).toBeFalse();
    expect(nodeMap[70]).toBeUndefined();
    expect(nodeMap[71]).toBeUndefined();
  });
});
