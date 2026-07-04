import { cookieGet, cookieGetAll, cookieSet, cookieDelete } from '../miso/util';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';

beforeAll(() => {
  console.log = () => {};
  console.error = () => {};
});

afterEach(() => {
  delete (globalThis as any).cookieStore;
});

/* helper: tick the promise queue */
const tick = () => new Promise(resolve => setTimeout(resolve, 10));

describe('cookieGet', () => {
  test('calls successful with cookie object when found', async () => {
    const cookie = { name: 'session', value: 'abc123' };
    (globalThis as any).cookieStore = {
      get: (_name: string) => Promise.resolve(cookie),
    };

    let result: any = undefined;
    let err: any = undefined;
    cookieGet('session', (e) => { err = e; }, (c) => { result = c; });
    await tick();

    expect(result).toBe(cookie.value);
    expect(err).toBeUndefined();
  });

  test('calls successful with null when cookie not found', async () => {
    (globalThis as any).cookieStore = {
      get: (_name: string) => Promise.resolve(null),
    };

    let result: any = 'sentinel';
    cookieGet('missing', () => {}, (c) => { result = c; });
    await tick();

    expect(result).toBeNull();
  });

  test('calls errorful on rejected promise', async () => {
    (globalThis as any).cookieStore = {
      get: (_name: string) => Promise.reject(new Error('permission denied')),
    };

    let errMsg: any = undefined;
    cookieGet('session', (e) => { errMsg = e; }, () => {});
    await tick();

    expect(errMsg).toBe('permission denied');
  });

  test('calls errorful when cookieStore throws synchronously', async () => {
    (globalThis as any).cookieStore = {
      get: () => { throw new Error('not in secure context'); },
    };

    let errMsg: any = undefined;
    cookieGet('session', (e) => { errMsg = e; }, () => {});
    await tick();

    expect(errMsg).toBe('not in secure context');
  });
});

describe('cookieGetAll', () => {
  test('calls successful with array of cookies', async () => {
    const cookies = [
      { name: 'a', value: '1' },
      { name: 'b', value: '2' },
    ];
    (globalThis as any).cookieStore = {
      getAll: () => Promise.resolve(cookies),
    };

    let result: any = undefined;
    cookieGetAll(() => {}, (cs) => { result = cs; });
    await tick();

    expect(result).toEqual(cookies);
  });

  test('calls successful with empty array when no cookies', async () => {
    (globalThis as any).cookieStore = {
      getAll: () => Promise.resolve([]),
    };

    let result: any = undefined;
    cookieGetAll(() => {}, (cs) => { result = cs; });
    await tick();

    expect(result).toEqual([]);
  });

  test('calls errorful on rejected promise', async () => {
    (globalThis as any).cookieStore = {
      getAll: () => Promise.reject(new Error('storage error')),
    };

    let errMsg: any = undefined;
    cookieGetAll((e) => { errMsg = e; }, () => {});
    await tick();

    expect(errMsg).toBe('storage error');
  });

  test('calls errorful when cookieStore throws synchronously', async () => {
    (globalThis as any).cookieStore = {
      getAll: () => { throw new Error('not available'); },
    };

    let errMsg: any = undefined;
    cookieGetAll((e) => { errMsg = e; }, () => {});
    await tick();

    expect(errMsg).toBe('not available');
  });
});

describe('cookieSet', () => {
  const cookie = { name: 'theme', value: 'dark', path: '/' };

  test('calls successful on resolution', async () => {
    (globalThis as any).cookieStore = {
      set: (_opts: any) => Promise.resolve(),
    };

    let called = false;
    cookieSet(cookie, () => {}, () => { called = true; });
    await tick();

    expect(called).toBe(true);
  });

  test('calls errorful on rejected promise', async () => {
    (globalThis as any).cookieStore = {
      set: () => Promise.reject(new Error('invalid cookie')),
    };

    let errMsg: any = undefined;
    cookieSet(cookie, (e) => { errMsg = e; }, () => {});
    await tick();

    expect(errMsg).toBe('invalid cookie');
  });

  test('calls errorful when cookieStore throws synchronously', async () => {
    (globalThis as any).cookieStore = {
      set: () => { throw new Error('not in secure context'); },
    };

    let errMsg: any = undefined;
    cookieSet(cookie, (e) => { errMsg = e; }, () => {});
    await tick();

    expect(errMsg).toBe('not in secure context');
  });

  test('passes cookie object through to cookieStore.set', async () => {
    let captured: any = null;
    (globalThis as any).cookieStore = {
      set: (opts: any) => { captured = opts; return Promise.resolve(); },
    };

    cookieSet(cookie, () => {}, () => {});
    await tick();

    expect(captured).toBe(cookie);
  });
});

describe('cookieDelete', () => {
  test('calls successful on resolution', async () => {
    (globalThis as any).cookieStore = {
      delete: (_name: string) => Promise.resolve(),
    };

    let called = false;
    cookieDelete('session', () => {}, () => { called = true; });
    await tick();

    expect(called).toBe(true);
  });

  test('calls errorful on rejected promise', async () => {
    (globalThis as any).cookieStore = {
      delete: () => Promise.reject(new Error('delete failed')),
    };

    let errMsg: any = undefined;
    cookieDelete('session', (e) => { errMsg = e; }, () => {});
    await tick();

    expect(errMsg).toBe('delete failed');
  });

  test('calls errorful when cookieStore throws synchronously', async () => {
    (globalThis as any).cookieStore = {
      delete: () => { throw new Error('not in secure context'); },
    };

    let errMsg: any = undefined;
    cookieDelete('session', (e) => { errMsg = e; }, () => {});
    await tick();

    expect(errMsg).toBe('not in secure context');
  });

  test('passes name as a string to cookieStore.delete', async () => {
    let captured: any = null;
    (globalThis as any).cookieStore = {
      delete: (arg: any) => { captured = arg; return Promise.resolve(); },
    };

    cookieDelete('auth_token', () => {}, () => {});
    await tick();

    expect(captured).toBe('auth_token');
  });

  test('does not hardcode a path option in the delete call', async () => {
    let captured: any = null;
    (globalThis as any).cookieStore = {
      delete: (arg: any) => { captured = arg; return Promise.resolve(); },
    };

    cookieDelete('session', () => {}, () => {});
    await tick();

    expect(typeof captured).toBe('string');
    expect(captured).not.toHaveProperty('path');
  });
});
