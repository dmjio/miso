/* Smoke tests for the MTS dual-thread event layer (ts/miso/native/mts/context.ts).
   These exercise the riskiest, previously-untested native logic: main-thread
   event routing (phase order + stopPropagation), the background-thread fallback,
   and per-node event-registry teardown. The Lynx PAPI globals (`__GetConfig`,
   `__AddEvent`) and the `runtime`/`lynx` host objects are stubbed here — in a web
   build they don't exist, which is exactly why this code has no other coverage. */
import { test, expect, describe, beforeEach, afterAll, beforeAll } from 'bun:test';
import { routeEvent, destroyNodeEvents, drawingContext } from '../miso/native/mts/context';
import type { EventContext } from '../miso/types';

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
    // mid's handler didn't request stopPropagation, so ancestor BTS handlers
    // must still get a chance to run — BTS's own delegateEvent skips
    // re-running mid's staticKey-tagged handler (see event.ts), so this
    // can't double-fire it.
    expect(btsDispatches.length).toBe(1);
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
