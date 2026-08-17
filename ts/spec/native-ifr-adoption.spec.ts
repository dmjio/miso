import { afterEach, describe, expect, test } from 'bun:test';
import { adoptInitialFrameNodeIds } from '../miso/native/mts/context';

type FakeNode = { name: string; nodeId: number };

function installRuntime(nodes: Record<number, FakeNode>): void {
  globalThis['runtime'] = { nodes };
  globalThis['nodeId'] = 100;
  globalThis['__SetConfig'] = ((node: FakeNode, config: { nodeId: number }) => {
    node.nodeId = config.nodeId;
  }) as any;
}

afterEach(() => {
  delete globalThis['runtime'];
  delete globalThis['__SetConfig'];
  globalThis['nodeId'] = 1;
});

describe('MTS initial-frame node-id adoption', () => {
  test('atomically rekeys runtime nodes and adopts the BTS allocator', () => {
    const root = { name: 'root', nodeId: 0 };
    const view = { name: 'view', nodeId: 41 };
    const text = { name: 'text', nodeId: 42 };
    installRuntime({ 0: root, 41: view, 42: text });

    adoptInitialFrameNodeIds(
      new Map([
        [0, 0],
        [41, 1],
        [42, 2],
      ]),
    );

    expect(globalThis['runtime'].nodes).toEqual({ 0: root, 1: view, 2: text });
    expect([root.nodeId, view.nodeId, text.nodeId]).toEqual([0, 1, 2]);
    expect(globalThis['nodeId']).toBe(3);
  });

  test('does not mutate live nodes when preflight finds an incomplete manifest', () => {
    const root = { name: 'root', nodeId: 0 };
    const view = { name: 'view', nodeId: 41 };
    installRuntime({ 0: root, 41: view });

    expect(() => adoptInitialFrameNodeIds(new Map([[0, 0]]))).toThrow(
      'runtime node count 2 != manifest node count 1',
    );

    expect(globalThis['runtime'].nodes).toEqual({ 0: root, 41: view });
    expect([root.nodeId, view.nodeId]).toEqual([0, 41]);
    expect(globalThis['nodeId']).toBe(100);
  });

  test('rolls back PAPI config if a later config write throws', () => {
    const root = { name: 'root', nodeId: 0 };
    const view = { name: 'view', nodeId: 41 };
    const text = { name: 'text', nodeId: 42 };
    installRuntime({ 0: root, 41: view, 42: text });
    globalThis['__SetConfig'] = ((node: FakeNode, config: { nodeId: number }) => {
      if (node === text && config.nodeId === 2) throw new Error('synthetic PAPI failure');
      node.nodeId = config.nodeId;
    }) as any;

    expect(() =>
      adoptInitialFrameNodeIds(
        new Map([
          [0, 0],
          [41, 1],
          [42, 2],
        ]),
      ),
    ).toThrow('synthetic PAPI failure');

    expect(globalThis['runtime'].nodes).toEqual({ 0: root, 41: view, 42: text });
    expect([root.nodeId, view.nodeId, text.nodeId]).toEqual([0, 41, 42]);
    expect(globalThis['nodeId']).toBe(100);
  });
});
