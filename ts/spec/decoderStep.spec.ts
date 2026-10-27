import { test, expect, describe } from 'bun:test';
import { decoderStep, decoderFailure } from '../miso/event';

describe('decoderStep', () => {
  test('reads a property, own or inherited', () => {
    const proto = { inherited: 2 };
    const obj = Object.create(proto);
    obj.own = 1;
    expect(decoderStep(obj, 'own', null)).toBe(1);
    expect(decoderStep(obj, 'inherited', null)).toBe(2);
    expect(decoderFailure(decoderStep(obj, 'own', null))).toBeNull();
  });

  test('reads a missing property as undefined, not a failure', () => {
    const r = decoderStep({}, 'missing', null);
    expect(r).toBeUndefined();
    expect(decoderFailure(r)).toBeNull();
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
      const f = decoderFailure(decoderStep(v, 'x', null));
      expect(f).not.toBeNull();
      expect(f!.nullish).toBeTrue();
    }
  });

  test('fails on a missing method', () => {
    const f = decoderFailure(decoderStep({ notAFunction: 1 }, 'notAFunction', []));
    expect(f).not.toBeNull();
    expect(f!.nullish).toBeFalse();
    expect(f!.message).toBe('no such method');
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
    expect(decoderFailure(decoderStep(obj, 'boom', null))!.message).toBe('threw Error: getter');
    expect(decoderFailure(decoderStep(obj, 'explode', []))!.message).toBe('threw Error: method');
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
    expect(decoderFailure(decoderStep(obj, 'explode', []))!.message).toBe('threw an exception');
  });
});
