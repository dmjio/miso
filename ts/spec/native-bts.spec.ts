/* Coverage for ts/miso/native/bts/context.ts's DrawingContext. Previously
   untested — the only other DrawingContext implementations exercised by the
   spec suite are the web DOM one (ts/miso/context/dom.ts) and the
   patch-testing one (ts/miso/context/patch.ts).

   nextSibling must match ts/miso/context/dom.ts's exact behavior: walk past
   VComp/VFrag siblings that render nothing, but STOP at a VNode/VText
   sibling and return its domRef as-is (even if null) — a VNode/VText is
   never "skipped past" the way an empty fragment/component is. This file
   caught two divergences from that in sequence: first a naive
   `getDOMRef(x.nextSibling)` one-liner (no walk at all), then a
   blanket "walk past any falsy domRef regardless of type" loop that
   incorrectly skipped past a VNode/VText with a null domRef instead of
   stopping there. */
import { test, expect, describe, beforeEach, afterEach } from 'bun:test';
import { drawingContext } from '../miso/native/bts/context';
import { diff } from '../miso/dom';
import { vnode, vfrag, vcomp, vtext } from '../miso/smart';
import type { NodeId } from '../miso/types';

describe('drawingContext.nextSibling — BTS', () => {

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

describe('drawingContext.insertBefore — BTS', () => {
  beforeEach(() => {
    (globalThis as any).nodeId = 1;
    (globalThis as any).patches = [];
  });
  afterEach(() => {
    delete (globalThis as any).nodeId;
    delete (globalThis as any).patches;
  });

  test('a null anchor degrades to an appendChild patch instead of crashing on `null.nodeId`', () => {
    const parent: NodeId = { nodeId: 100 }, child: NodeId = { nodeId: 101 };
    expect(() => drawingContext.insertBefore(parent, child, null as any)).not.toThrow();
    const recorded = (globalThis as any).patches;
    expect(recorded).toEqual([{ type: 'appendChild', parent: 100, child: 101 }]);
  });

  test('a real anchor still records an insertBefore patch', () => {
    const parent: NodeId = { nodeId: 110 }, child: NodeId = { nodeId: 111 }, anchor: NodeId = { nodeId: 112 };
    drawingContext.insertBefore(parent, child, anchor);
    const recorded = (globalThis as any).patches;
    expect(recorded).toEqual([{ type: 'insertBefore', parent: 110, child: 112, node: 111 }]);
  });

  test('mounting a VNode containing a non-empty VFrag child does not crash', () => {
    // dom.ts's `create` always mounts fresh content via createElement with a
    // null "replacing" anchor; a VFrag forwards that anchor unchanged to each
    // child's insertBefore call. Regression test for the crash this produced
    // before insertBefore learned to degrade a null anchor to appendChild.
    const tree = vnode<NodeId>({
      tag: 'view',
      children: [vfrag<NodeId>([vtext('a'), vtext('b')])],
    });
    const parent: NodeId = { nodeId: 0 };
    expect(() => diff(null as any, tree, parent, drawingContext)).not.toThrow();
  });
});
