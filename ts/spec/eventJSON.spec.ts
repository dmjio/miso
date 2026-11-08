import { test, expect, describe, afterEach } from 'bun:test';
import { eventJSON } from '../miso/event';

/* reset DOM */
afterEach(() => {
  document.body.innerHTML = '';
});

describe('eventJSON', () => {
  test('extracts primitives at path from plain object', () => {
    const obj = { a: { b: { x: 1, y: 's', z: false, nested: { q: {} }, fn: () => 1 } } };
    const res: any = eventJSON(['a', 'b'], obj);
    expect(Array.isArray(res)).toBeFalse();
    expect(res).toEqual({ x: 1, y: 's', z: false });
  });

  test('handles list-like array by mapping elements', () => {
    const obj = { list: [{ k: 1, s: 'a' }, { k: 2, s: 'b' }] };
    const res: any = eventJSON(['list'], obj);
    expect(Array.isArray(res)).toBeTrue();
    expect(res).toEqual([{ k: 1, s: 'a' }, { k: 2, s: 'b' }]);
  });

  test('treats non-select length-bearing object as list-like', () => {
    const pseudoList: any = { length: 2, 0: { k: 1 }, 1: { k: 2 }, localName: 'div' };
    const res: any = eventJSON([], pseudoList);
    expect(Array.isArray(res)).toBeTrue();
    expect(res).toEqual([{ k: 1 }, { k: 2 }]);
  });

  test('does not treat select element as list-like', () => {
    const select = document.createElement('select');
    // add a couple of options to ensure length > 0
    select.appendChild(document.createElement('option'));
    select.appendChild(document.createElement('option'));
    const wrap = { target: select };
    const res: any = eventJSON(['target'], wrap);
    expect(Array.isArray(res)).toBeFalse();
    // spot-check a couple of primitive fields exist
    expect(typeof res.localName).toBe('string');
    expect(typeof res.length).toBe('number');
  });

  test('skips input selection fields for Safari bug guard', () => {
    const input = document.createElement('input');
    input.value = 'abc';
    // happy-dom exposes selection* props; call selection methods if present
    try { (input as any).setSelectionRange?.(0, 1); } catch {}
    const wrap = { target: input } as any;
    const res: any = eventJSON(['target'], wrap);
    expect(res.selectionStart).toBeUndefined();
    expect(res.selectionEnd).toBeUndefined();
    expect(res.selectionDirection).toBeUndefined();
    // but ordinary primitives should be present
    expect(res.localName).toBe('input');
    expect(typeof res.type).toBe('string');
  });

  test('accepts nested at arrays ([[path]]) and returns tuple of results', () => {
    const obj = { list: [{ k: 1 }, { k: 2 }], a: { b: { p: true } } } as any;
    const res: any = eventJSON([['list'] as any, ['a', 'b'] as any] as any, obj);
    expect(Array.isArray(res)).toBeTrue();
    expect(res.length).toBe(2);
    expect(res[0]).toEqual([{ k: 1 }, { k: 2 }]);
    expect(res[1]).toEqual({ p: true });
  });

  test('calls a method segment with its arguments', () => {
    const obj = {
      clipboardData: { getData: (fmt: string) => (fmt === 'text/plain' ? 'hello' : '') },
    };
    expect(eventJSON(['clipboardData', { call: 'getData', args: ['text/plain'] }], obj)).toBe('hello');
    expect(eventJSON(['clipboardData', { call: 'getData', args: ['text/html'] }], obj)).toBe('');
  });

  test('calls a zero-argument method and serializes the returned object (getters included)', () => {
    class Rect {
      constructor(public x: number, public y: number, public width: number, public height: number) {}
      get left() { return this.x; }
      get top() { return this.y; }
    }
    const el = { getBoundingClientRect: () => new Rect(1, 2, 3, 4) };
    const res: any = eventJSON(['target', { call: 'getBoundingClientRect', args: [] }], { target: el });
    expect(res).toEqual({ x: 1, y: 2, width: 3, height: 4, left: 1, top: 2 });
  });

  test('method is invoked with the object as `this`', () => {
    const obj = { n: 7, getN() { return this.n; } };
    expect(eventJSON([{ call: 'getN', args: [] }], obj)).toBe(7);
  });

  test('a missing method decodes as null instead of throwing', () => {
    expect(eventJSON(['target', { call: 'nope', args: [] }], { target: {} })).toBeNull();
  });

  test('a method returning null/undefined decodes as null', () => {
    const obj = { target: { closest: (_: string) => null } };
    expect(eventJSON(['target', { call: 'closest', args: ['.card'] }], obj)).toBeNull();
  });

  test('"currentTarget" is rooted at the element argument, not the event', () => {
    const event = { currentTarget: { id: 'mount-point' } };
    const element = { id: 'handler-element', getBoundingClientRect: () => ({ x: 5 }) };
    expect(eventJSON(['currentTarget'], event, element)).toEqual({ id: 'handler-element' });
    expect(eventJSON(['currentTarget', { call: 'getBoundingClientRect', args: [] }], event, element)).toEqual({ x: 5 });
    /* without an element it is the plain event property */
    expect(eventJSON(['currentTarget'], event)).toEqual({ id: 'mount-point' });
  });

  test('returns primitive results directly', () => {
    const obj = { s: 'str', n: 3, b: true, list: ['a', 'b'] };
    expect(eventJSON(['s'], obj)).toBe('str');
    expect(eventJSON(['n'], obj)).toBe(3);
    expect(eventJSON(['b'], obj)).toBe(true);
    expect(eventJSON(['list'], obj)).toEqual(['a', 'b']);
  });

  test('a path landing on a function (not called) decodes as null', () => {
    expect(eventJSON(['target', 'focus'], { target: { focus: () => 1 } })).toBeNull();
  });

  test('DecodeTargets with method segments decodes each path', () => {
    const event = { clientX: 10, getModifierState: (k: string) => k === 'CapsLock' };
    const element = { getBoundingClientRect: () => ({ left: 4 }) };
    const res: any = eventJSON(
      [[], ['currentTarget', { call: 'getBoundingClientRect', args: [] }], [{ call: 'getModifierState', args: ['CapsLock'] }]],
      event,
      element,
    );
    expect(res).toEqual([{ clientX: 10 }, { left: 4 }, true]);
  });

  // Regression: a path landing on null/undefined (relatedTarget, currentTarget,
  // form, list, etc. are all legitimately null/undefined on real DOM events)
  // used to throw `'length' in obj` on the final null/undefined value instead
  // of decoding it as null.
  test('decodes a path whose final value is null as null, without throwing', () => {
    const obj = { target: { relatedTarget: null } };
    expect(eventJSON(['target', 'relatedTarget'], obj)).toBeNull();
  });

  test('decodes a path whose final value is undefined as null, without throwing', () => {
    const obj = { target: {} } as any;
    expect(eventJSON(['target', 'missing'], obj)).toBeNull();
  });

  // A path can also go nullish *before* the last step (e.g. `at.form` is null
  // on an input not inside a <form>, and the decoder still asks for a field
  // past it) -- the walk must not dereference a property off that null.
  test('does not throw when an intermediate path segment is null', () => {
    const obj = { target: { form: null } };
    expect(() => eventJSON(['target', 'form', 'id'], obj)).not.toThrow();
    expect(eventJSON(['target', 'form', 'id'], obj)).toBeNull();
  });

  test('still decodes a present path normally after the null-safety change', () => {
    const obj = { target: { form: { id: 'my-form' } } };
    const res: any = eventJSON(['target', 'form'], obj);
    expect(res).toEqual({ id: 'my-form' });
  });
});
