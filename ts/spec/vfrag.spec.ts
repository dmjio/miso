/**
 * Tests for VFrag (React-Fragment-like) virtual DOM nodes.
 *
 * VFrag represents a group of sibling nodes with no enclosing DOM element.
 */
import { diff } from '../miso/dom';
import { delegator } from '../miso/event';
import { hydrate } from '../miso/hydrate';
import { vfrag, vfragKeyed, vcomp, vnode, vtext, vtextKeyed } from '../miso/smart';
import { DOMRef, VNode, VText, EventCapture } from '../miso/types';
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

// ---------------------------------------------------------------------------
// DOM diffing tests
// ---------------------------------------------------------------------------

describe('VFrag — DOM creation', () => {

  test('creates all children as siblings with no wrapper element', () => {
    const frag = vfrag<DOMRef>([vtext('a'), vtext('b'), vtext('c')]);
    diff<DOMRef>(null, frag, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(3);
    expect(document.body.childNodes[0].textContent).toBe('a');
    expect(document.body.childNodes[1].textContent).toBe('b');
    expect(document.body.childNodes[2].textContent).toBe('c');
  });

  test('creates fragment containing element nodes', () => {
    const frag = vfrag<DOMRef>([
      vnode<DOMRef>({ tag: 'span' }),
      vnode<DOMRef>({ tag: 'p' }),
    ]);
    diff<DOMRef>(null, frag, document.body, drawingContext);
    expect(document.body.children.length).toBe(2);
    expect(document.body.children[0].tagName.toLowerCase()).toBe('span');
    expect(document.body.children[1].tagName.toLowerCase()).toBe('p');
  });

  test('creates empty fragment (no children added)', () => {
    const frag = vfrag<DOMRef>([]);
    diff<DOMRef>(null, frag, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(0);
  });

  test('creates nested fragment (fragment inside fragment)', () => {
    const inner = vfrag<DOMRef>([vtext('x'), vtext('y')]);
    const outer = vfrag<DOMRef>([vtext('before'), inner, vtext('after')]);
    diff<DOMRef>(null, outer, document.body, drawingContext);
    // 'before', 'x', 'y', 'after' — 4 text nodes
    expect(document.body.childNodes.length).toBe(4);
    expect(document.body.textContent).toBe('beforexyafter');
  });

});

// ---------------------------------------------------------------------------
// Keyed VFrag children in the syncChildren (child reconciliation) path
// ---------------------------------------------------------------------------

describe('VFrag — keyed children sync (syncChildren)', () => {

  // Helper: render a VNode whose children are keyed VFrags, then re-render
  // with a new child order and return the resulting DOM text content array.
  function textOrder(): string[] {
    return Array.from(document.body.firstChild!.childNodes).map(n => n.textContent ?? '');
  }

  test('happy path — same order, in-place diff of keyed fragment children', () => {
    // fA: [a1, a2]  fB: [b1, b2]   ->  same order, update text
    const cA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const cB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const old = vnode<DOMRef>({ tag: 'div', children: [cA, cB] });
    diff(null, old, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'b1', 'b2']);

    const nA = vfragKeyed<DOMRef>('A', [vtext('A1'), vtext('A2')]);
    const nB = vfragKeyed<DOMRef>('B', [vtext('B1'), vtext('B2')]);
    const next = vnode<DOMRef>({ tag: 'div', children: [nA, nB] });
    diff(old, next, document.body, drawingContext);
    expect(textOrder()).toEqual(['A1', 'A2', 'B1', 'B2']);
  });

  test('flip-flop swap — two keyed fragments fully reversed', () => {
    // [ fA fB ] -> [ fB fA ]
    // fA = [a1, a2], fB = [b1, b2]
    const cA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const cB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const old = vnode<DOMRef>({ tag: 'div', children: [cA, cB] });
    diff(null, old, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'b1', 'b2']);

    const nB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const nA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const next = vnode<DOMRef>({ tag: 'div', children: [nB, nA] });
    diff(old, next, document.body, drawingContext);
    expect(textOrder()).toEqual(['b1', 'b2', 'a1', 'a2']);
  });

  test('move first to last — [ fA fB fC ] -> [ fB fC fA ]', () => {
    const cA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const cB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const cC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const old = vnode<DOMRef>({ tag: 'div', children: [cA, cB, cC] });
    diff(null, old, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'b1', 'b2', 'c1', 'c2']);

    const nB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const nC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const nA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const next = vnode<DOMRef>({ tag: 'div', children: [nB, nC, nA] });
    diff(old, next, document.body, drawingContext);
    expect(textOrder()).toEqual(['b1', 'b2', 'c1', 'c2', 'a1', 'a2']);
  });

  test('move last to first — [ fA fB fC ] -> [ fC fA fB ]', () => {
    const cA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const cB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const cC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const old = vnode<DOMRef>({ tag: 'div', children: [cA, cB, cC] });
    diff(null, old, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'b1', 'b2', 'c1', 'c2']);

    const nC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const nA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const nB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const next = vnode<DOMRef>({ tag: 'div', children: [nC, nA, nB] });
    diff(old, next, document.body, drawingContext);
    expect(textOrder()).toEqual(['c1', 'c2', 'a1', 'a2', 'b1', 'b2']);
  });

  test('insert new keyed fragment into existing list (case 1: no more old nodes)', () => {
    // [ fA fC ] -> [ fA fB fC ]  — fB is new, inserted between fA and fC
    const cA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const cC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const old = vnode<DOMRef>({ tag: 'div', children: [cA, cC] });
    diff(null, old, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'c1', 'c2']);

    const nA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const nB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const nC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const next = vnode<DOMRef>({ tag: 'div', children: [nA, nB, nC] });
    diff(old, next, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'b1', 'b2', 'c1', 'c2']);
  });

  test('delete keyed fragment from middle of list (case 2: no more new nodes)', () => {
    // [ fA fB fC ] -> [ fA fC ]  — fB is removed
    const cA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const cB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const cC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const old = vnode<DOMRef>({ tag: 'div', children: [cA, cB, cC] });
    diff(null, old, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'b1', 'b2', 'c1', 'c2']);

    const nA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const nC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const next = vnode<DOMRef>({ tag: 'div', children: [nA, nC] });
    diff(old, next, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'c1', 'c2']);
  });

  test('linear search — sorted shuffle (case 8: no bilateral key match)', () => {
    // [ fA fE fC ] -> [ fB fE fD ]
    // fA and fC are removed, fB and fD are new, fE stays in place via linear search
    const cA = vfragKeyed<DOMRef>('A', [vtext('a1'), vtext('a2')]);
    const cE = vfragKeyed<DOMRef>('E', [vtext('e1'), vtext('e2')]);
    const cC = vfragKeyed<DOMRef>('C', [vtext('c1'), vtext('c2')]);
    const old = vnode<DOMRef>({ tag: 'div', children: [cA, cE, cC] });
    diff(null, old, document.body, drawingContext);
    expect(textOrder()).toEqual(['a1', 'a2', 'e1', 'e2', 'c1', 'c2']);

    const nB = vfragKeyed<DOMRef>('B', [vtext('b1'), vtext('b2')]);
    const nE = vfragKeyed<DOMRef>('E', [vtext('e1'), vtext('e2')]);
    const nD = vfragKeyed<DOMRef>('D', [vtext('d1'), vtext('d2')]);
    const next = vnode<DOMRef>({ tag: 'div', children: [nB, nE, nD] });
    diff(old, next, document.body, drawingContext);
    expect(textOrder()).toEqual(['b1', 'b2', 'e1', 'e2', 'd1', 'd2']);
  });

});

// ---------------------------------------------------------------------------
// Nested keyed fragments — fragments whose children are themselves keyed
// fragments, exercising the syncChildren key optimizations recursively.
// ---------------------------------------------------------------------------

describe('VFrag — nested keyed fragments (syncChildren recursion)', () => {

  function bodyText(): string[] {
    return Array.from(document.body.firstChild!.childNodes).map(n => n.textContent ?? '');
  }

  test('nested keyed frags: in-place update of inner keyed children', () => {
    // outer frag contains two inner keyed frags, each with keyed text children
    // vtextKeyed(text, key) — keys are stable ('ka','kb','kc','kd'), only text changes
    const inner1 = vfragKeyed<DOMRef>('I1', [
      vtextKeyed('a', 'ka'),
      vtextKeyed('b', 'kb'),
    ]);
    const inner2 = vfragKeyed<DOMRef>('I2', [
      vtextKeyed('c', 'kc'),
      vtextKeyed('d', 'kd'),
    ]);
    const outer = vfragKeyed<DOMRef>('O', [inner1, inner2]);
    const root = vnode<DOMRef>({ tag: 'div', children: [outer] });
    diff(null, root, document.body, drawingContext);
    expect(bodyText()).toEqual(['a', 'b', 'c', 'd']);

    // update text values in place — keys are stable so syncChildren should patch
    const nInner1 = vfragKeyed<DOMRef>('I1', [
      vtextKeyed('A', 'ka'),
      vtextKeyed('B', 'kb'),
    ]);
    const nInner2 = vfragKeyed<DOMRef>('I2', [
      vtextKeyed('C', 'kc'),
      vtextKeyed('D', 'kd'),
    ]);
    const nOuter = vfragKeyed<DOMRef>('O', [nInner1, nInner2]);
    const nRoot = vnode<DOMRef>({ tag: 'div', children: [nOuter] });
    diff(root, nRoot, document.body, drawingContext);
    expect(bodyText()).toEqual(['A', 'B', 'C', 'D']);
  });

  test('nested keyed frags: swap inner keyed fragments inside outer', () => {
    // [ outer[ I1[a,b], I2[c,d] ] ] -> [ outer[ I2[c,d], I1[a,b] ] ]
    const inner1 = vfragKeyed<DOMRef>('I1', [vtext('a'), vtext('b')]);
    const inner2 = vfragKeyed<DOMRef>('I2', [vtext('c'), vtext('d')]);
    const outer = vfragKeyed<DOMRef>('O', [inner1, inner2]);
    const root = vnode<DOMRef>({ tag: 'div', children: [outer] });
    diff(null, root, document.body, drawingContext);
    expect(bodyText()).toEqual(['a', 'b', 'c', 'd']);

    const nInner2 = vfragKeyed<DOMRef>('I2', [vtext('c'), vtext('d')]);
    const nInner1 = vfragKeyed<DOMRef>('I1', [vtext('a'), vtext('b')]);
    const nOuter = vfragKeyed<DOMRef>('O', [nInner2, nInner1]);
    const nRoot = vnode<DOMRef>({ tag: 'div', children: [nOuter] });
    diff(root, nRoot, document.body, drawingContext);
    expect(bodyText()).toEqual(['c', 'd', 'a', 'b']);
  });

  test('nested keyed frags: swap outer keyed fragments', () => {
    // div[ F1[ I1[a,b], I2[c,d] ], F2[ I3[e,f] ] ]
    // ->
    // div[ F2[ I3[e,f] ], F1[ I1[a,b], I2[c,d] ] ]
    const i1 = vfragKeyed<DOMRef>('I1', [vtext('a'), vtext('b')]);
    const i2 = vfragKeyed<DOMRef>('I2', [vtext('c'), vtext('d')]);
    const i3 = vfragKeyed<DOMRef>('I3', [vtext('e'), vtext('f')]);
    const f1 = vfragKeyed<DOMRef>('F1', [i1, i2]);
    const f2 = vfragKeyed<DOMRef>('F2', [i3]);
    const root = vnode<DOMRef>({ tag: 'div', children: [f1, f2] });
    diff(null, root, document.body, drawingContext);
    expect(bodyText()).toEqual(['a', 'b', 'c', 'd', 'e', 'f']);

    const ni1 = vfragKeyed<DOMRef>('I1', [vtext('a'), vtext('b')]);
    const ni2 = vfragKeyed<DOMRef>('I2', [vtext('c'), vtext('d')]);
    const ni3 = vfragKeyed<DOMRef>('I3', [vtext('e'), vtext('f')]);
    const nf1 = vfragKeyed<DOMRef>('F1', [ni1, ni2]);
    const nf2 = vfragKeyed<DOMRef>('F2', [ni3]);
    const nRoot = vnode<DOMRef>({ tag: 'div', children: [nf2, nf1] });
    diff(root, nRoot, document.body, drawingContext);
    expect(bodyText()).toEqual(['e', 'f', 'a', 'b', 'c', 'd']);
  });

});

describe('VFrag — DOM diffing (same key, in-place update)', () => {

  test('diffs two fragments with same key, updating text children', () => {
    const c = vfragKeyed<DOMRef>('k', [vtext('old1'), vtext('old2')]);
    diff<DOMRef>(null, c, document.body, drawingContext);
    expect(document.body.childNodes[0].textContent).toBe('old1');
    expect(document.body.childNodes[1].textContent).toBe('old2');

    const n = vfragKeyed<DOMRef>('k', [vtext('new1'), vtext('new2')]);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(2);
    expect(document.body.childNodes[0].textContent).toBe('new1');
    expect(document.body.childNodes[1].textContent).toBe('new2');
  });

  test('diffs fragment against itself (no-op), DOM unchanged', () => {
    const c = vfrag<DOMRef>([vtext('x')]);
    diff<DOMRef>(null, c, document.body, drawingContext);
    const ref = document.body.childNodes[0];

    const n = vfrag<DOMRef>([vtext('x')]);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);
    // text node reference should be reused
    expect(document.body.childNodes[0].textContent).toBe('x');
  });

  test('replaces fragment with different key', () => {
    const c = vfragKeyed<DOMRef>('k1', [vtext('a'), vtext('b')]);
    diff<DOMRef>(null, c, document.body, drawingContext);

    const n = vfragKeyed<DOMRef>('k2', [vtext('c')]);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);
    expect(document.body.childNodes[0].textContent).toBe('c');
  });

  test('replaces fragment with different key preserves position when followed by a sibling', () => {
    // [ frag(k1)[a,b], span ]  ->  [ frag(k2)[c], span ]
    // Without the anchor fix, frag(k2) would be appended after span.
    const c = vfragKeyed<DOMRef>('k1', [vtext('a'), vtext('b')]);
    const sibling = vnode<DOMRef>({ tag: 'span' });
    const parent = vnode<DOMRef>({ tag: 'div', children: [c, sibling] });
    diff<DOMRef>(null, parent, document.body, drawingContext);
    expect((document.body.firstChild as Element).childNodes.length).toBe(3); // a, b, span

    const n = vfragKeyed<DOMRef>('k2', [vtext('c')]);
    const nSibling = vnode<DOMRef>({ tag: 'span' });
    const nParent = vnode<DOMRef>({ tag: 'div', children: [n, nSibling] });
    diff<DOMRef>(parent, nParent, document.body, drawingContext);

    const div = document.body.firstChild as Element;
    expect(div.childNodes.length).toBe(2); // c, span
    expect(div.childNodes[0].textContent).toBe('c');
    expect((div.childNodes[1] as Element).tagName.toLowerCase()).toBe('span');
  });

});

describe('VFrag — DOM diffing (replace with other types)', () => {

  test('replaces VFrag with VNode', () => {
    const c = vfrag<DOMRef>([vtext('a'), vtext('b')]);
    diff<DOMRef>(null, c, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(2);

    const n = vnode<DOMRef>({ tag: 'div' });
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);
    expect((document.body.childNodes[0] as Element).tagName.toLowerCase()).toBe('div');
  });

  test('replaces VNode with VFrag', () => {
    const c = vnode<DOMRef>({ tag: 'div' });
    diff<DOMRef>(null, c, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);

    const n = vfrag<DOMRef>([vtext('p'), vtext('q')]);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(2);
    expect(document.body.childNodes[0].textContent).toBe('p');
    expect(document.body.childNodes[1].textContent).toBe('q');
  });

  test('replaces VNode with VFrag preserves position when followed by a sibling', () => {
    // [ div(to-replace), span ] -> [ frag[a,b], span ]
    // Without the fix, frag children were appended after span.
    const c = vnode<DOMRef>({ tag: 'div' });
    const sibling = vnode<DOMRef>({ tag: 'span' });
    const parent = vnode<DOMRef>({ tag: 'section', children: [c, sibling] });
    diff<DOMRef>(null, parent, document.body, drawingContext);
    expect((document.body.firstChild as Element).childNodes.length).toBe(2);

    const n = vfrag<DOMRef>([vtext('a'), vtext('b')]);
    const nSibling = vnode<DOMRef>({ tag: 'span' });
    const nParent = vnode<DOMRef>({ tag: 'section', children: [n, nSibling] });
    diff<DOMRef>(parent, nParent, document.body, drawingContext);

    const section = document.body.firstChild as Element;
    expect(section.childNodes.length).toBe(3); // a, b, span
    expect(section.childNodes[0].textContent).toBe('a');
    expect(section.childNodes[1].textContent).toBe('b');
    expect((section.childNodes[2] as Element).tagName.toLowerCase()).toBe('span');
  });

  test('replaces VFrag with VText', () => {
    const c = vfrag<DOMRef>([vtext('x'), vtext('y')]);
    diff<DOMRef>(null, c, document.body, drawingContext);

    const n = vtext<DOMRef>('hello');
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);
    expect(document.body.textContent).toBe('hello');
  });

  test('replaces VFrag with VNode preserves position when followed by a sibling', () => {
    // [ frag[a,b], span ] -> [ div, span ]
    // Without the anchor fix, div would be appended after span.
    const c = vfrag<DOMRef>([vtext('a'), vtext('b')]);
    const sibling = vnode<DOMRef>({ tag: 'span' });
    const parent = vnode<DOMRef>({ tag: 'div', children: [c, sibling] });
    diff<DOMRef>(null, parent, document.body, drawingContext);
    expect((document.body.firstChild as Element).childNodes.length).toBe(3); // a, b, span

    const n = vnode<DOMRef>({ tag: 'p' });
    const nSibling = vnode<DOMRef>({ tag: 'span' });
    const nParent = vnode<DOMRef>({ tag: 'div', children: [n, nSibling] });
    diff<DOMRef>(parent, nParent, document.body, drawingContext);

    const div = document.body.firstChild as Element;
    expect(div.childNodes.length).toBe(2); // p, span
    expect((div.childNodes[0] as Element).tagName.toLowerCase()).toBe('p');
    expect((div.childNodes[1] as Element).tagName.toLowerCase()).toBe('span');
  });

});

describe('VFrag — DOM removal', () => {

  test('destroys all children when diffed against null', () => {
    const c = vfrag<DOMRef>([vtext('a'), vtext('b')]);
    diff<DOMRef>(null, c, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(2);

    diff<DOMRef>(c, null, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(0);
  });

});

// ---------------------------------------------------------------------------
// Event delegation tests
// ---------------------------------------------------------------------------

describe('VFrag — event delegation', () => {

  test('delegates click to a VNode child inside a VFrag', () => {
    var count = 0;
    const btn = vnode<DOMRef>({
      tag: 'button',
      events: {
        captures: {},
        bubbles: {
          click: {
            options: { preventDefault: false, stopPropagation: false },
            runEvent: (_e, _node) => { count++; },
          },
        },
      },
    });
    const frag = vfrag<DOMRef>([btn]);
    // set parent links
    btn.parent = frag as any;

    diff<DOMRef>(null, frag, document.body, drawingContext);

    const events: Array<EventCapture> = [{ name: 'click', capture: false }];
    delegator<DOMRef>(
      document.body,
      events,
      (cb) => cb(frag),
      false,
      eventContext,
    );

    (btn.domRef as HTMLElement).click();
    expect(count).toBe(1);
  });

  test('event bubbles through VFrag parent', () => {
    var outerCount = 0;
    const inner = vnode<DOMRef>({
      tag: 'button',
      events: {
        captures: {},
        bubbles: {
          click: {
            options: { preventDefault: false, stopPropagation: false },
            runEvent: (_e, _node) => {},
          },
        },
      },
    });
    const outer = vnode<DOMRef>({
      tag: 'div',
      events: {
        captures: {},
        bubbles: {
          click: {
            options: { preventDefault: false, stopPropagation: false },
            runEvent: (_e, _node) => { outerCount++; },
          },
        },
      },
      children: [],
    });
    const frag = vfrag<DOMRef>([inner]);

    // wire up parent chain: inner -> frag -> outer
    inner.parent = frag as any;
    frag.parent = outer as any;
    outer.children = [frag as any];

    diff<DOMRef>(null, outer, document.body, drawingContext);
    // manually place button inside outer's dom
    outer.domRef.appendChild(inner.domRef);

    const events: Array<EventCapture> = [{ name: 'click', capture: false }];
    delegator<DOMRef>(
      document.body,
      events,
      (cb) => cb(outer),
      false,
      eventContext,
    );

    (inner.domRef as HTMLElement).click();
    expect(outerCount).toBe(1);
  });

});

// ---------------------------------------------------------------------------
// Empty VFrag edge cases
// ---------------------------------------------------------------------------

describe('VFrag — empty fragment edge cases', () => {

  test('drill on empty VFrag throws (empty frags are normalised to null in Haskell)', () => {
    const { drill } = require('../miso/util');
    const frag = vfrag<DOMRef>([]);
    expect(() => drill(frag)).toThrow();
  });

  test('creating an empty fragment leaves DOM unchanged', () => {
    const frag = vfrag<DOMRef>([]);
    diff<DOMRef>(null, frag, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(0);
  });

  test('destroying an empty fragment is a no-op', () => {
    document.body.appendChild(document.createTextNode('bystander'));
    const frag = vfrag<DOMRef>([]);
    diff<DOMRef>(frag, null, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);
    expect(document.body.childNodes[0].textContent).toBe('bystander');
  });

  test('replacing non-empty node with empty fragment removes the node', () => {
    const c = vnode<DOMRef>({ tag: 'div' });
    diff<DOMRef>(null, c, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);

    const n = vfrag<DOMRef>([]);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(0);
  });

  test('replacing empty fragment with a node adds the node', () => {
    const c = vfrag<DOMRef>([]);
    diff<DOMRef>(null, c, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(0);

    const n = vnode<DOMRef>({ tag: 'span' });
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);
    expect((document.body.childNodes[0] as Element).tagName.toLowerCase()).toBe('span');
  });

  test('diffing two empty fragments with same key is a no-op', () => {
    const c = vfragKeyed<DOMRef>('k', []);
    diff<DOMRef>(null, c, document.body, drawingContext);
    const n = vfragKeyed<DOMRef>('k', []);
    diff<DOMRef>(c, n, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(0);
  });

});

describe('VFrag — hydration', () => {

  test('hydrates fragment of text nodes from pre-rendered DOM', () => {
    document.body.appendChild(document.createTextNode('hello'));
    document.body.appendChild(document.createTextNode(' world'));

    const frag = vfrag<DOMRef>([vtext('hello'), vtext(' world')]);
    const result = hydrate(false, document.body, frag, hydrationContext, drawingContext);
    expect(result).toBe(true);
    expect((frag.children[0] as VText<DOMRef>).domRef.textContent).toBe('hello');
    expect((frag.children[1] as VText<DOMRef>).domRef.textContent).toBe(' world');
  });

  test('hydrates fragment of element nodes', () => {
    const span = document.createElement('span');
    const p = document.createElement('p');
    document.body.appendChild(span);
    document.body.appendChild(p);

    const frag = vfrag<DOMRef>([
      vnode<DOMRef>({ tag: 'span' }),
      vnode<DOMRef>({ tag: 'p' }),
    ]);
    const result = hydrate(false, document.body, frag, hydrationContext, drawingContext);
    expect(result).toBe(true);
    expect((frag.children[0] as VNode<DOMRef>).domRef).toBe(span);
    expect((frag.children[1] as VNode<DOMRef>).domRef).toBe(p);
  });

  test('hydration fails when DOM has fewer nodes than fragment children', () => {
    document.body.appendChild(document.createTextNode('only one'));

    const frag = vfrag<DOMRef>([vtext('only one'), vtext('missing')]);
    const result = hydrate(false, document.body, frag, hydrationContext, drawingContext);
    expect(result).toBe(false);
  });

});

// ---------------------------------------------------------------------------
// VComp whose root is a VFrag
// ---------------------------------------------------------------------------

describe('VComp with VFrag root', () => {

  test('mounts all fragment children as siblings', () => {
    const comp = vcomp<DOMRef>({
      mount: (p) => {
        const frag = vfrag<DOMRef>([vtext('x'), vtext('y'), vtext('z')]);
        diff(null, frag, p, drawingContext);
        return { componentId: 0, componentTree: frag };
      },
    });
    diff(null, comp, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(3);
    expect(document.body.childNodes[0].textContent).toBe('x');
    expect(document.body.childNodes[1].textContent).toBe('y');
    expect(document.body.childNodes[2].textContent).toBe('z');
  });

  test('mounts fragment of element nodes', () => {
    const comp = vcomp<DOMRef>({
      mount: (p) => {
        const frag = vfrag<DOMRef>([
          vnode<DOMRef>({ tag: 'span' }),
          vnode<DOMRef>({ tag: 'div' }),
        ]);
        diff(null, frag, p, drawingContext);
        return { componentId: 1, componentTree: frag };
      },
    });
    diff(null, comp, document.body, drawingContext);
    expect(document.body.children.length).toBe(2);
    expect(document.body.children[0].tagName.toLowerCase()).toBe('span');
    expect(document.body.children[1].tagName.toLowerCase()).toBe('div');
  });

  test('nextSibling drills into first child of VFrag-rooted component', () => {
    const comp = vcomp<DOMRef>({
      mount: (p) => {
        const frag = vfrag<DOMRef>([vnode<DOMRef>({ tag: 'span' }), vnode<DOMRef>({ tag: 'em' })]);
        diff(null, frag, p, drawingContext);
        return { componentId: 2, componentTree: frag };
      },
    });
    const sibling = vnode<DOMRef>({ tag: 'b' });
    comp.nextSibling = sibling;
    const parent = vnode<DOMRef>({ tag: 'div', children: [comp, sibling] });
    diff(null, parent, document.body, drawingContext);
    // nextSibling of the comp should be the domRef of the sibling 'b'
    expect(drawingContext.nextSibling(comp)).toBe((sibling as VNode<DOMRef>).domRef);
  });

  test('replacing a VComp whose root is VFrag with a VNode removes all fragment children', () => {
    const comp = vcomp<DOMRef>({
      mount: (p) => {
        const frag = vfrag<DOMRef>([vtext('a'), vtext('b')]);
        diff(null, frag, p, drawingContext);
        return { componentId: 3, componentTree: frag };
      },
      unmount: (_id) => {},
    });
    diff(null, comp, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(2);

    // replace with a plain node
    const next = vnode<DOMRef>({ tag: 'p' });
    diff(comp, next, document.body, drawingContext);
    expect(document.body.children.length).toBe(1);
    expect(document.body.children[0].tagName.toLowerCase()).toBe('p');
  });

  test('VComp with VFrag root inserted before a sibling places all fragment children in correct position', () => {
    // Render a div with [ "before", comp(root=frag["x","y"]), "after" ].
    // The comp is diffed via INSERT_BEFORE (anchor = "after" text node).
    // All of x and y must appear between "before" and "after", not at the end.
    const comp = vcomp<DOMRef>({
      mount: (p) => {
        const frag = vfrag<DOMRef>([vtext('x'), vtext('y')]);
        diff(null, frag, p, drawingContext);
        return { componentId: 4, componentTree: frag };
      },
    });
    const before = vtext<DOMRef>('before');
    const after = vtext<DOMRef>('after');
    const parent = vnode<DOMRef>({ tag: 'div', children: [before, comp, after] });
    diff(null, parent, document.body, drawingContext);

    const div = document.body.firstChild as Element;
    // Expected order: before | x | y | after  (4 child nodes)
    expect(div.childNodes.length).toBe(4);
    expect(div.childNodes[0].textContent).toBe('before');
    expect(div.childNodes[1].textContent).toBe('x');
    expect(div.childNodes[2].textContent).toBe('y');
    expect(div.childNodes[3].textContent).toBe('after');
  });

  test('destroying a VComp whose root is VFrag removes all fragment children from the DOM', () => {
    // Render [ "before", comp(root=frag["p","q","r"]), "after" ] then destroy the comp.
    // All 3 fragment DOM nodes must be removed, leaving only the two surrounding text nodes.
    const comp = vcomp<DOMRef>({
      mount: (p) => {
        const frag = vfrag<DOMRef>([vtext('p'), vtext('q'), vtext('r')]);
        diff(null, frag, p, drawingContext);
        return { componentId: 5, componentTree: frag };
      },
      unmount: (_id) => {},
    });
    const before = vtext<DOMRef>('before');
    const after = vtext<DOMRef>('after');
    const parent = vnode<DOMRef>({ tag: 'div', children: [before, comp, after] });
    diff(null, parent, document.body, drawingContext);

    const div = document.body.firstChild as Element;
    expect(div.childNodes.length).toBe(5); // before, p, q, r, after

    // Destroy the component — all 3 fragment nodes must go, leaving before + after
    diff(comp, null, div as unknown as DOMRef, drawingContext);
    expect(div.childNodes.length).toBe(2);
    expect(div.childNodes[0].textContent).toBe('before');
    expect(div.childNodes[1].textContent).toBe('after');
  });

});
