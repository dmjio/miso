import { test, expect, describe } from 'bun:test';
import { decoderStep, decoderFailed, decoderError, decoderSnapshot, decoderPick } from '../miso/event';

describe('decoderStep', () => {
  test('reads a property, own or inherited', () => {
    const proto = { inherited: 2 };
    const obj = Object.create(proto);
    obj.own = 1;
    expect(decoderStep(obj, 'own', null)).toBe(1);
    expect(decoderStep(obj, 'inherited', null)).toBe(2);
  });

  test('reads a missing property as undefined, not a failure', () => {
    const r = decoderStep({}, 'missing', null);
    expect(r).toBeUndefined();
    expect(r).not.toBe(decoderFailed);
  });

  test('reads properties of primitives', () => {
    expect(decoderStep('abc', 'length', null)).toBe(3);
  });

  test('calls a method with its arguments and this', () => {
    const obj = {
      k: 1,
      add(a: number, b: number) {
        return a + b + this.k;
      },
    };
    expect(decoderStep(obj, 'add', [2, 3])).toBe(6);
  });

  test('fails, flagged nullish, on null and undefined', () => {
    for (const v of [null, undefined]) {
      expect(decoderStep(v, 'x', null)).toBe(decoderFailed);
      expect(decoderError().nullish).toBeTrue();
    }
  });

  test('fails on a missing method', () => {
    expect(decoderStep({ notAFunction: 1 }, 'notAFunction', [])).toBe(decoderFailed);
    expect(decoderError().nullish).toBeFalse();
    expect(decoderError().message).toBe('no such method');
  });

  test('turns an exception from a getter or a method into a failure', () => {
    const obj = {
      get boom() {
        throw new Error('getter');
      },
      explode() {
        throw new Error('method');
      },
    };
    expect(decoderStep(obj, 'boom', null)).toBe(decoderFailed);
    expect(decoderError().message).toBe('threw Error: getter');
    expect(decoderStep(obj, 'explode', [])).toBe(decoderFailed);
    expect(decoderError().message).toBe('threw Error: method');
  });

  test('survives an exception that cannot be turned into a string', () => {
    const obj = {
      explode() {
        throw {
          toString() {
            throw new Error('nope');
          },
        };
      },
    };
    expect(decoderStep(obj, 'explode', [])).toBe(decoderFailed);
    expect(decoderError().message).toBe('threw an exception');
  });
});

describe('decoderSnapshot', () => {
  test('passes primitives through and turns null and undefined into null', () => {
    expect(decoderSnapshot('a')).toBe('a');
    expect(decoderSnapshot(1)).toBe(1);
    expect(decoderSnapshot(false)).toBe(false);
    expect(decoderSnapshot(null)).toBeNull();
    expect(decoderSnapshot(undefined)).toBeNull();
  });

  test('keeps the string, number and boolean properties of an object', () => {
    expect(decoderSnapshot({ s: 'a', n: 1, b: true, o: {}, f: () => 1 })).toEqual({ s: 'a', n: 1, b: true });
  });
});

describe('decoderPick', () => {
  test('keeps only the named string, number and boolean properties', () => {
    const proto = { inherited: 2 };
    const obj = Object.create(proto);
    obj.own = 'a';
    obj.other = 3;
    obj.object = {};
    expect(decoderPick(obj, ['own', 'inherited', 'object', 'missing'])).toEqual({ own: 'a', inherited: 2 });
  });
});
