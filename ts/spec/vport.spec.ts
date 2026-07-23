/**
 * Tests for VPort (React-Portal-like) virtual DOM nodes.
 *
 * A VPort renders its `child` into an out-of-tree DOM node (`location`) that is
 * not managed by the diffing algorithm, while occupying zero DOM in its host
 * parent. Events still trace through the virtual tree (delegation + bubbling).
 */
import { diff } from '../miso/dom';
import { delegator } from '../miso/event';
import { hydrate } from '../miso/hydrate';
import { getFirstDOMRef, getLastDOMRef } from '../miso/util';
import { vport, vportKeyed, vfrag, vnode, vtext } from '../miso/smart';
import { DOMRef, EventCapture } from '../miso/types';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { drawingContext, eventContext, hydrationContext } from '../miso/context/dom';

beforeAll(() => {
  console.log = () => {};
  console.info = () => {};
  console.warn = () => {};
  console.error = () => {};
});

afterEach(() => {
  document.body.innerHTML = '';
});

// A fresh out-of-tree container to portal into.
function makeLocation(): DOMRef {
  const loc = document.createElement('div');
  document.body.appendChild(loc);
  return loc as unknown as DOMRef;
}

// ---------------------------------------------------------------------------
// DOM creation
// ---------------------------------------------------------------------------

describe('VPort — DOM creation', () => {

  test('renders child into location, not the host parent', () => {
    const location = makeLocation();
    const host = document.createElement('main');
    document.body.appendChild(host);

    const port = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), location);
    diff<DOMRef>(null, port, host as unknown as DOMRef, drawingContext);

    // Nothing landed in the host parent...
    expect(host.childNodes.length).toBe(0);
    // ...the child rendered under location instead.
    expect(location.childNodes.length).toBe(1);
    expect((location.childNodes[0] as Element).tagName.toLowerCase()).toBe('span');
  });

  test('child parent pointer is wired back to the portal', () => {
    const location = makeLocation();
    const child = vnode<DOMRef>({ tag: 'span' });
    const port = vport<DOMRef>(child, location);
    diff<DOMRef>(null, port, document.body, drawingContext);
    expect(child.parent).toBe(port as any);
  });

  test('null location warns and leaves host untouched (no throw)', () => {
    const host = document.createElement('main');
    document.body.appendChild(host);
    const port = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), null as unknown as DOMRef);
    expect(() => diff<DOMRef>(null, port, host as unknown as DOMRef, drawingContext)).not.toThrow();
    expect(host.childNodes.length).toBe(0);
  });

  test('portal contributes zero DOM to a host list of siblings', () => {
    const location = makeLocation();
    const host = vnode<DOMRef>({ tag: 'div', children: [
      vtext<DOMRef>('a'),
      vport<DOMRef>(vnode<DOMRef>({ tag: 'b' }), location),
      vtext<DOMRef>('c'),
    ]});
    diff<DOMRef>(null, host, document.body, drawingContext);
    const div = host.domRef as unknown as Element;
    // Only 'a' and 'c' live in the host; the portal's <b> is under location.
    expect(Array.from(div.childNodes).map(n => n.textContent)).toEqual(['a', 'c']);
    expect(location.childNodes.length).toBe(1);
    expect((location.childNodes[0] as Element).tagName.toLowerCase()).toBe('b');
  });

});

// ---------------------------------------------------------------------------
// DOM-ref helpers: a portal has no presence in its host parent
// ---------------------------------------------------------------------------

describe('VPort — DOM-ref helpers', () => {

  test('getFirstDOMRef / getLastDOMRef return null (zero host presence)', () => {
    const location = makeLocation();
    const port = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), location);
    diff<DOMRef>(null, port, document.body, drawingContext);
    expect(getFirstDOMRef(port)).toBeNull();
    expect(getLastDOMRef(port)).toBeNull();
  });

  test('a portal between two siblings does not disturb their adjacency', () => {
    const location = makeLocation();
    const host = document.createElement('div');
    document.body.appendChild(host);
    const parent = vnode<DOMRef>({ tag: 'div', children: [
      vnode<DOMRef>({ tag: 'i' }),
      vport<DOMRef>(vnode<DOMRef>({ tag: 'em' }), location),
      vnode<DOMRef>({ tag: 'u' }),
    ]});
    diff<DOMRef>(null, parent, host as unknown as DOMRef, drawingContext);
    const div = host.firstChild as Element;
    expect(Array.from(div.children).map(e => e.tagName.toLowerCase())).toEqual(['i', 'u']);
  });

});

// ---------------------------------------------------------------------------
// DOM diffing — same key + same location diffs in place
// ---------------------------------------------------------------------------

describe('VPort — DOM diffing (same key & location)', () => {

  test('updates the portal child in place at location', () => {
    const location = makeLocation();
    const c = vportKeyed<DOMRef>('k', vnode<DOMRef>({ tag: 'p', children: [vtext('old')] }), location);
    diff<DOMRef>(null, c, document.body, drawingContext);
    const pRef = location.childNodes[0];
    expect(pRef.textContent).toBe('old');

    const n = vportKeyed<DOMRef>('k', vnode<DOMRef>({ tag: 'p', children: [vtext('new')] }), location);
    diff<DOMRef>(c, n, document.body, drawingContext);
    // same DOM element reused, text updated
    expect(location.childNodes.length).toBe(1);
    expect(location.childNodes[0]).toBe(pRef);
    expect(location.childNodes[0].textContent).toBe('new');
  });

  test('diffing two null-location portals does not crash (falls through to replace)', () => {
    const c = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), null as unknown as DOMRef);
    diff<DOMRef>(null, c, document.body, drawingContext); // warns, draws nothing
    const n = vport<DOMRef>(vnode<DOMRef>({ tag: 'p' }), null as unknown as DOMRef);
    expect(() => diff<DOMRef>(c, n, document.body, drawingContext)).not.toThrow();
    expect(document.body.childNodes.length).toBe(0);
  });

  test('keyless portal against keyless portal at same location diffs in place', () => {
    const location = makeLocation();
    const c = vport<DOMRef>(vtext<DOMRef>('x'), location);
    diff<DOMRef>(null, c, document.body, drawingContext);
    const ref = location.childNodes[0];

    const n = vport<DOMRef>(vtext<DOMRef>('y'), location);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(location.childNodes.length).toBe(1);
    expect(location.childNodes[0]).toBe(ref); // text node reused
    expect(location.childNodes[0].textContent).toBe('y');
  });

});

// ---------------------------------------------------------------------------
// DOM diffing — a changed location or key is a replace
// ---------------------------------------------------------------------------

describe('VPort — DOM diffing (replace)', () => {

  test('moving to a different location tears down the old and populates the new', () => {
    const locA = makeLocation();
    const locB = makeLocation();
    const c = vportKeyed<DOMRef>('k', vnode<DOMRef>({ tag: 'span' }), locA);
    diff<DOMRef>(null, c, document.body, drawingContext);
    expect(locA.childNodes.length).toBe(1);

    const n = vportKeyed<DOMRef>('k', vnode<DOMRef>({ tag: 'span' }), locB);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(locA.childNodes.length).toBe(0); // old location cleared
    expect(locB.childNodes.length).toBe(1); // new location populated
  });

  test('a different key is a replace at the (same) location', () => {
    const location = makeLocation();
    const c = vportKeyed<DOMRef>('k1', vnode<DOMRef>({ tag: 'span', children: [vtext('a')] }), location);
    diff<DOMRef>(null, c, document.body, drawingContext);

    const n = vportKeyed<DOMRef>('k2', vnode<DOMRef>({ tag: 'p', children: [vtext('b')] }), location);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(location.childNodes.length).toBe(1);
    expect((location.childNodes[0] as Element).tagName.toLowerCase()).toBe('p');
    expect(location.textContent).toBe('b');
  });

  test('replaces a VNode with a VPort (host node removed, child sent to location)', () => {
    const location = makeLocation();
    const host = document.createElement('div');
    document.body.appendChild(host);
    const c = vnode<DOMRef>({ tag: 'div' });
    diff<DOMRef>(null, c, host as unknown as DOMRef, drawingContext);
    expect(host.childNodes.length).toBe(1);

    const n = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), location);
    diff<DOMRef>(c, n, host as unknown as DOMRef, drawingContext);
    expect(host.childNodes.length).toBe(0);       // old VNode removed from host
    expect(location.childNodes.length).toBe(1);   // portal child now under location
  });

  test('replaces a VPort with a VNode (location cleared, host populated)', () => {
    const location = makeLocation();
    const host = document.createElement('div');
    document.body.appendChild(host);
    const c = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), location);
    diff<DOMRef>(null, c, host as unknown as DOMRef, drawingContext);
    expect(location.childNodes.length).toBe(1);

    const n = vnode<DOMRef>({ tag: 'p' });
    diff<DOMRef>(c, n, host as unknown as DOMRef, drawingContext);
    expect(location.childNodes.length).toBe(0);   // portal torn down at location
    expect(host.childNodes.length).toBe(1);       // VNode created in host
    expect((host.childNodes[0] as Element).tagName.toLowerCase()).toBe('p');
  });

  test('replacing a portal preserves host position when followed by a sibling', () => {
    // [ portal(->loc), span ] -> [ p, span ]. The <p> must land before <span>.
    const location = makeLocation();
    const host = document.createElement('div');
    document.body.appendChild(host);
    const c = vport<DOMRef>(vnode<DOMRef>({ tag: 'em' }), location);
    const sibling = vnode<DOMRef>({ tag: 'span' });
    // The runtime wires nextSibling on children (Runtime.hs setNextSibling); a
    // portal has no host DOM, so it must find its host anchor via that pointer.
    c.nextSibling = sibling;
    const parent = vnode<DOMRef>({ tag: 'section', children: [c, sibling] });
    diff<DOMRef>(null, parent, host as unknown as DOMRef, drawingContext);
    // host section holds only the span (portal has no host DOM)
    expect((host.firstChild as Element).childNodes.length).toBe(1);

    const n = vnode<DOMRef>({ tag: 'p' });
    const nSibling = vnode<DOMRef>({ tag: 'span' });
    const nParent = vnode<DOMRef>({ tag: 'section', children: [n, nSibling] });
    diff<DOMRef>(parent, nParent, host as unknown as DOMRef, drawingContext);

    const section = host.firstChild as Element;
    expect(Array.from(section.children).map(e => e.tagName.toLowerCase())).toEqual(['p', 'span']);
    expect(location.childNodes.length).toBe(0);
  });

});

// ---------------------------------------------------------------------------
// DOM removal
// ---------------------------------------------------------------------------

describe('VPort — DOM removal', () => {

  test('destroying a portal removes its child from location', () => {
    const location = makeLocation();
    const c = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), location);
    diff<DOMRef>(null, c, document.body, drawingContext);
    expect(location.childNodes.length).toBe(1);

    diff<DOMRef>(c, null, document.body, drawingContext);
    expect(location.childNodes.length).toBe(0);
  });

  // Regression (bug #1): destroying a host subtree that CONTAINS a nested portal
  // must tear the portal down at its location — the host DOM removal never
  // touched it. Covers the diff→null (pure destroy) path.
  test('destroying a host subtree tears down a nested portal at location', () => {
    const location = makeLocation();
    const host = document.createElement('div');
    document.body.appendChild(host);
    const parent = vnode<DOMRef>({ tag: 'section', children: [
      vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), location),
    ]});
    diff<DOMRef>(null, parent, host as unknown as DOMRef, drawingContext);
    expect((parent.domRef as unknown as Element).childNodes.length).toBe(0);
    expect(location.childNodes.length).toBe(1);

    diff<DOMRef>(parent, null, host as unknown as DOMRef, drawingContext);
    expect(host.childNodes.length).toBe(0);
    expect(location.childNodes.length).toBe(0); // portal cleaned up, not leaked
  });

  // Regression (bug #1): the same leak via the *replace* path — swapping the
  // host subtree for a different node must still tear down the nested portal.
  test('replacing a host subtree that contains a portal cleans up location', () => {
    const location = makeLocation();
    const destroyed = { n: 0 };
    const parent = vnode<DOMRef>({ tag: 'div', children: [
      vport<DOMRef>(
        vnode<DOMRef>({ tag: 'span', onDestroyed: () => { destroyed.n++; } }),
        location,
      ),
    ]});
    diff<DOMRef>(null, parent, document.body, drawingContext);
    expect(location.childNodes.length).toBe(1);

    const next = vnode<DOMRef>({ tag: 'p' });
    diff<DOMRef>(parent, next, document.body, drawingContext);
    expect(location.childNodes.length).toBe(0); // no leak
    expect(destroyed.n).toBe(1);                // child onDestroyed fired
  });

});

// ---------------------------------------------------------------------------
// Keyed reconciliation — a portal has no host DOM, so reordering it among
// keyed siblings must not move its child out of location (bug #2).
// ---------------------------------------------------------------------------

describe('VPort — keyed reorder', () => {

  test('reordering a keyed portal among siblings leaves its DOM in location', () => {
    const location = makeLocation();
    const host = document.createElement('div');
    document.body.appendChild(host);

    const port = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), location, 'k1');
    const b = vnode<DOMRef>({ tag: 'b', key: 'k2' });
    const parent = vnode<DOMRef>({ tag: 'div', children: [port, b] });
    diff<DOMRef>(null, parent, host as unknown as DOMRef, drawingContext);
    const hostDiv = parent.domRef as unknown as Element;
    expect(hostDiv.childNodes.length).toBe(1);      // only <b> in host
    expect(location.childNodes.length).toBe(1);     // portal's <span> in location

    // reorder to [ b(k2), portal(k1) ]
    const port2 = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), location, 'k1');
    const b2 = vnode<DOMRef>({ tag: 'b', key: 'k2' });
    const parent2 = vnode<DOMRef>({ tag: 'div', children: [b2, port2] });
    diff<DOMRef>(parent, parent2, host as unknown as DOMRef, drawingContext);

    // Portal DOM stayed put; host still holds only <b> (not the portal's span).
    expect(location.childNodes.length).toBe(1);
    expect(hostDiv.childNodes.length).toBe(1);
    expect((hostDiv.childNodes[0] as Element).tagName.toLowerCase()).toBe('b');
  });

});

// ---------------------------------------------------------------------------
// Event delegation — events trace through the virtual tree into the portal
// ---------------------------------------------------------------------------

describe('VPort — event delegation', () => {

  function clickHandler(counter: { n: number }) {
    return {
      options: { preventDefault: false, stopPropagation: false },
      runEvent: (_e: any, _node: any) => { counter.n++; },
    };
  }

  test('delegates a click into a portal whose child is the event target (stack length 1)', () => {
    const count = { n: 0 };
    // Portal into body so the child button is a direct child of the delegator mount.
    const btn = vnode<DOMRef>({
      tag: 'button',
      events: { captures: {}, bubbles: { click: clickHandler(count) } },
    });
    const port = vport<DOMRef>(btn, document.body as unknown as DOMRef);
    const root = vfrag<DOMRef>([port]);
    btn.parent = port as any;
    port.parent = root as any;

    diff<DOMRef>(null, root, document.body, drawingContext);

    const events: Array<EventCapture> = [{ name: 'click', capture: false }];
    delegator<DOMRef>(document.body, events, (cb) => cb(root), false, eventContext);

    (btn.domRef as HTMLElement).click();
    expect(count.n).toBe(1);
  });

  test('delegates a click into a nested portal child (stack length > 1)', () => {
    const count = { n: 0 };
    const btn = vnode<DOMRef>({
      tag: 'button',
      events: { captures: {}, bubbles: { click: clickHandler(count) } },
    });
    const wrapper = vnode<DOMRef>({ tag: 'div', children: [btn] });
    const port = vport<DOMRef>(wrapper, document.body as unknown as DOMRef);
    const root = vfrag<DOMRef>([port]);
    btn.parent = wrapper as any;
    wrapper.parent = port as any;
    port.parent = root as any;

    diff<DOMRef>(null, root, document.body, drawingContext);

    const events: Array<EventCapture> = [{ name: 'click', capture: false }];
    delegator<DOMRef>(document.body, events, (cb) => cb(root), false, eventContext);

    (btn.domRef as HTMLElement).click();
    expect(count.n).toBe(1);
  });

  // Regression (sample-app "+1 from portal"): the portal target is a real DOM
  // container nested inside the mount (like <div id="portal-root"> in <body>),
  // introducing an unmanaged DOM level between mount and target. Delegation must
  // skip that container and still reach the handler.
  test('delegates a click through an unmanaged location container nested in the mount', () => {
    const count = { n: 0 };
    const location = document.createElement('div'); // e.g. #portal-root, child of body
    document.body.appendChild(location);

    const btn = vnode<DOMRef>({
      tag: 'button',
      events: { captures: {}, bubbles: { click: clickHandler(count) } },
    });
    const wrapper = vnode<DOMRef>({ tag: 'div', children: [btn] });
    const port = vport<DOMRef>(wrapper, location as unknown as DOMRef);
    const root = vfrag<DOMRef>([port]);
    btn.parent = wrapper as any;
    wrapper.parent = port as any;
    port.parent = root as any;

    diff<DOMRef>(null, root, document.body, drawingContext);
    // DOM is body > location > div > button — two levels below the mount.
    expect(location.querySelector('button')).toBe(btn.domRef as unknown as HTMLButtonElement);

    const events: Array<EventCapture> = [{ name: 'click', capture: false }];
    delegator<DOMRef>(document.body, events, (cb) => cb(root), false, eventContext);

    (btn.domRef as HTMLElement).click();
    expect(count.n).toBe(1);
  });

  test('event bubbles out of the portal into its virtual (host) parent', () => {
    const inner = { n: 0 };
    const outer = { n: 0 };
    const btn = vnode<DOMRef>({
      tag: 'button',
      events: { captures: {}, bubbles: { click: clickHandler(inner) } },
    });
    const port = vport<DOMRef>(btn, document.body as unknown as DOMRef);
    // Virtual host ancestor holding a bubble handler — reached only through the
    // vtree (its DOM is irrelevant; propagateWhileAble walks vtree.parent).
    const hostAncestor = vnode<DOMRef>({
      tag: 'div',
      events: { captures: {}, bubbles: { click: clickHandler(outer) } },
      children: [port],
    });
    btn.parent = port as any;
    port.parent = hostAncestor as any;

    diff<DOMRef>(null, port, document.body, drawingContext);

    const events: Array<EventCapture> = [{ name: 'click', capture: false }];
    delegator<DOMRef>(document.body, events, (cb) => cb(port), false, eventContext);

    (btn.domRef as HTMLElement).click();
    expect(inner.n).toBe(1);
    expect(outer.n).toBe(1); // propagateWhileAble hopped through the VPort to its parent
  });

});

// ---------------------------------------------------------------------------
// Hydration — child hydrates against pre-existing DOM under location
// ---------------------------------------------------------------------------

describe('VPort — hydration', () => {

  test('draws the portal child into location during hydration (server renders nothing there)', () => {
    // renderBuilder emits nothing for a portal, so location exists but is empty.
    const location = document.createElement('div');
    document.body.appendChild(location);

    const child = vnode<DOMRef>({ tag: 'span' });
    const port = vport<DOMRef>(child, location as unknown as DOMRef);
    const root = vfrag<DOMRef>([port]);

    const result = hydrate(false, document.body, root, hydrationContext, drawingContext);
    expect(result).toBe(true);
    // child was drawn fresh into the (empty) location
    expect(location.childNodes.length).toBe(1);
    expect(child.domRef).toBe(location.childNodes[0] as unknown as DOMRef);
    expect(child.parent).toBe(port as any);
  });

  // Regression: an empty location must NOT crash hydration (was a null deref on
  // firstChild(location).nodeType). This is the normal SSR case for a portal.
  test('empty location does not crash hydration and the rest of the page still hydrates', () => {
    // Host DOM: a single <p> sibling; the portal's location is empty.
    const p = document.createElement('p');
    document.body.appendChild(p);
    const location = document.createElement('div');
    document.body.appendChild(location);

    const portChild = vnode<DOMRef>({ tag: 'span' });
    const pNode = vnode<DOMRef>({ tag: 'p' });
    // host root: [ portal, p ] — the portal consumes no host DOM.
    const root = vfrag<DOMRef>([
      vport<DOMRef>(portChild, location as unknown as DOMRef),
      pNode,
    ]);

    let threw: string | null = null;
    let result = false;
    try {
      result = hydrate(false, document.body, root, hydrationContext, drawingContext);
    } catch (e) {
      threw = (e as Error).message;
    }
    expect(threw).toBeNull();                                   // no crash
    expect(result).toBe(true);                                  // page hydrated
    expect(pNode.domRef).toBe(p as unknown as DOMRef);          // host sibling matched <p>
    expect(location.childNodes.length).toBe(1);                 // portal drawn into location
    expect(portChild.domRef).toBe(location.childNodes[0] as unknown as DOMRef);
  });

  test('hydration fails gracefully when location is null', () => {
    const p = document.createElement('p');
    document.body.appendChild(p);
    const root = vport<DOMRef>(vnode<DOMRef>({ tag: 'span' }), null as unknown as DOMRef);
    const result = hydrate(false, document.body, root, hydrationContext, drawingContext);
    expect(result).toBe(false);
  });

  // Regression: a portal must NOT be drawn if the walk later fails. Otherwise the
  // caller's fallback redraw would duplicate the portal content in location.
  test('a portal is not drawn when hydration fails mid-walk (no stray/duplicate DOM)', () => {
    const location = document.createElement('div');
    document.body.appendChild(location); // a single element node in the host
    // The fragment's 2nd child is a VText, which cannot hydrate against an element
    // node → walk fails *after* the portal (1st child) has been queued.
    const root = vfrag<DOMRef>([
      vport<DOMRef>(vnode<DOMRef>({ tag: 'em' }), location as unknown as DOMRef),
      vtext<DOMRef>('nope'), // element node ≠ text node → walk returns false
    ]);

    const result = hydrate(false, document.body, root, hydrationContext, drawingContext);
    expect(result).toBe(false);
    // Because the walk failed, the deferred portal draw must NOT have happened.
    expect(location.childNodes.length).toBe(0);
  });

});
