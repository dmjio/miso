import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { patchDrawingContext, getPatches } from '../miso/context/patch';
import { DrawingContext, NodeId } from '../miso/types';

beforeAll(() => {
  console.log = () => {};
  console.info = () => {};
  console.warn = () => {};
  console.error = () => {};
  globalThis['componentId'] = 0;
  globalThis['nodeId'] = 1;
  globalThis['patches'] = [];
});

afterEach(() => {
  globalThis['patches'] = [];
  globalThis['nodeId'] = 1;
});

describe('Patch DrawingContext micro-tests', () => {
  test('setInlineStyle does not emit when equal', () => {
    const ctx: DrawingContext<NodeId> = patchDrawingContext;
    const node = { nodeId: 1 } as NodeId;
    ctx.setInlineStyle({ color: 'red' }, { color: 'red' }, node);
    expect(getPatches().length).toBe(0);
  });

  test('flush clears queued patches', () => {
    const ctx: DrawingContext<NodeId> = patchDrawingContext;
    const parent = { nodeId: 0 } as NodeId;
    const child = ctx.createElement('div');
    ctx.appendChild(parent, child);
    expect(getPatches().length).toBeGreaterThan(0);
    ctx.flush();
    expect(getPatches().length).toBe(0);
  });

  test('getRoot returns nodeId 0', () => {
    const root = patchDrawingContext.getRoot();
    expect(root.nodeId).toBe(0);
  });

  test('setAttribute and removeAttribute emit patches', () => {
    const parent = { nodeId: 0 } as NodeId;
    const child = patchDrawingContext.createElement('div');
    patchDrawingContext.appendChild(parent, child);
    patchDrawingContext.setAttribute(child, 'title', 'hello');
    patchDrawingContext.removeAttribute(child, 'title');
    const patches = getPatches();
    const types = patches.map(p => p.type);
    expect(types).toContain('setAttribute');
    expect(types).toContain('removeAttribute');
  });

  test('setAttributeNS emits patch', () => {
    const node = patchDrawingContext.createElementNS('http://www.w3.org/2000/svg', 'svg');
    patchDrawingContext.setAttributeNS(node, 'http://www.w3.org/1999/xlink', 'href', 'x');
    const last = getPatches()[getPatches().length - 1];
    expect(last.type).toBe('setAttributeNS');
    expect((last as any).namespace).toBe('http://www.w3.org/1999/xlink');
  });

  test('addClass and removeClass emit patches', () => {
    const node = patchDrawingContext.createElement('div');
    patchDrawingContext.addClass('foo', node);
    patchDrawingContext.removeClass('foo', node);
    const types = getPatches().map(p => p.type);
    expect(types).toContain('addClass');
    expect(types).toContain('removeClass');
  });
});
