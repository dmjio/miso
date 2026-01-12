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
});
