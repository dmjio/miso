import { onBTS, onMTS } from '../miso/util';
import { test, expect, describe } from 'bun:test';

describe('onBTS', () => {

  test('returns false in a web build where __BACKGROUND__ is undefined', () => {
    // In a web/test environment __BACKGROUND__ is never defined by rspeedy,
    // so onBTS() must safely return false without throwing.
    expect(onBTS()).toBe(false);
  });

  test('returns true when __BACKGROUND__ is true (simulated Lynx BTS)', () => {
    (globalThis as any).__BACKGROUND__ = true;
    expect(onBTS()).toBe(true);
    delete (globalThis as any).__BACKGROUND__;
  });

  test('returns false when __BACKGROUND__ is false (simulated Lynx MTS)', () => {
    (globalThis as any).__BACKGROUND__ = false;
    expect(onBTS()).toBe(false);
    delete (globalThis as any).__BACKGROUND__;
  });

});

describe('onMTS', () => {

  test('returns false in a web build where __MAIN_THREAD__ is undefined', () => {
    expect(onMTS()).toBe(false);
  });

  test('returns true when __MAIN_THREAD__ is true (simulated Lynx MTS)', () => {
    (globalThis as any).__MAIN_THREAD__ = true;
    expect(onMTS()).toBe(true);
    delete (globalThis as any).__MAIN_THREAD__;
  });

  test('returns false when __MAIN_THREAD__ is false (simulated Lynx BTS)', () => {
    (globalThis as any).__MAIN_THREAD__ = false;
    expect(onMTS()).toBe(false);
    delete (globalThis as any).__MAIN_THREAD__;
  });

});
