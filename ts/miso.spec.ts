/* imports */
import { version } from './miso/util';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';

/* silence */
beforeAll(() => {
  console.log = () => {};
  console.info = () => {};
  console.warn = () => {};
  console.error = () => {};
});

/* reset DOM */
afterEach(() => {
  document.body.innerHTML = '';
});

/* tests */
describe('Version test', () => {
  test('Should be latest version', () => {
    expect(version).toEqual('1.9.0.0');
  });
});
