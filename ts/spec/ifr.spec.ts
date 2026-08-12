import { describe, expect, test } from 'bun:test';
import type { PATCH } from '../miso/types';
import {
  MAX_DEFERRED_INITIAL_FRAME_EVENTS,
  InitialFrameRecorder,
  InitialFrameReconciler,
  compareInitialFrames,
  type InitialFrameMessage,
} from '../miso/native/ifr';

function recordFrame(recorder: InitialFrameRecorder, firstNodeId: number): void {
  const secondNodeId = firstNodeId + 1;
  const patches: Array<PATCH> = [
    { type: 'createElement', nodeId: firstNodeId, tag: 'view' },
    { type: 'setAttribute', nodeId: firstNodeId, key: 'id', value: 'root-child' },
    { type: 'createTextNode', nodeId: secondNodeId, text: 'hello' },
    { type: 'appendChild', parent: firstNodeId, child: secondNodeId },
    { type: 'appendChild', parent: 0, child: firstNodeId },
    {
      type: 'addEvent',
      nodeId: firstNodeId,
      name: 'tap',
      capture: false,
      componentId: 7,
      staticKey: 'abc',
      options: { preventDefault: false, stopPropagation: true },
    },
    { type: 'flush' },
  ];
  for (const patch of patches) recorder.record(patch);
}

describe('initial-frame manifest', () => {
  test('compares structure by creation slot and returns BTS-authoritative ids', () => {
    const main = new InitialFrameRecorder();
    const background = new InitialFrameRecorder();
    recordFrame(main, 41);
    recordFrame(background, 1);

    const result = compareInitialFrames(main.snapshot('main'), background.snapshot('background'));

    expect(result.ok).toBe(true);
    if (result.ok) {
      expect([...result.nodeIds.entries()]).toEqual([
        [0, 0],
        [41, 1],
        [42, 2],
      ]);
    }
  });

  test('reports the first structural mismatch', () => {
    const main = new InitialFrameRecorder();
    const background = new InitialFrameRecorder();
    recordFrame(main, 1);
    recordFrame(background, 1);
    background.record({ type: 'createElement', nodeId: 3, tag: 'image' });

    const result = compareInitialFrames(main.snapshot('main'), background.snapshot('background'));

    expect(result).toEqual({ ok: false, reason: 'node count 3 != 4' });
  });

  test('canonicalizes object-valued attributes independent of key insertion order', () => {
    const main = new InitialFrameRecorder();
    const background = new InitialFrameRecorder();
    main.record({ type: 'createElement', nodeId: 1, tag: 'view' });
    background.record({ type: 'createElement', nodeId: 9, tag: 'view' });
    main.record({ type: 'setAttribute', nodeId: 1, key: 'payload', value: { z: 1, a: 2 } });
    background.record({ type: 'setAttribute', nodeId: 9, key: 'payload', value: { a: 2, z: 1 } });

    expect(compareInitialFrames(main.snapshot('main'), background.snapshot('background')).ok).toBe(
      true,
    );
  });

  test('rejects references to a node that was never created', () => {
    const recorder = new InitialFrameRecorder();
    expect(() => recorder.record({ type: 'appendChild', parent: 0, child: 99 })).toThrow(
      'initial-frame reference to unknown nodeId 99',
    );
  });
});

describe('initial-frame reconciliation state machine', () => {
  test('adopts before ACK and drains BTS patches only after ACK', () => {
    const mainRecorder = new InitialFrameRecorder();
    const backgroundRecorder = new InitialFrameRecorder();
    recordFrame(mainRecorder, 41);
    recordFrame(backgroundRecorder, 1);

    const mainMessages: Array<InitialFrameMessage> = [];
    const backgroundMessages: Array<InitialFrameMessage> = [];
    const adopted: Array<Array<[number, number]>> = [];
    const mainPatches: Array<Array<string>> = [];
    const backgroundPatches: Array<Array<string>> = [];

    const main = new InitialFrameReconciler<Array<string>>(
      'main',
      mainRecorder,
      { send: (message) => backgroundMessages.push(message) },
      {
        adoptNodeIds: (ids) => adopted.push([...ids.entries()]),
        deliverPatches: (patches) => mainPatches.push(patches),
      },
    );
    const background = new InitialFrameReconciler<Array<string>>(
      'background',
      backgroundRecorder,
      { send: (message) => mainMessages.push(message) },
      { deliverPatches: (patches) => backgroundPatches.push(patches) },
    );

    main.finalize('main-local');
    main.receiveOrQueuePatches(['early-main']);
    background.sendOrQueuePatches(['early-background']);
    background.finalize('session-1');
    expect(background.state).toBe('awaiting-peer');
    expect(backgroundPatches).toEqual([]);

    main.receive(mainMessages.shift()!);
    expect(main.state).toBe('adopted');
    expect(main.status().session).toBe('session-1');
    expect(adopted).toEqual([
      [
        [0, 0],
        [41, 1],
        [42, 2],
      ],
    ]);
    expect(mainPatches).toEqual([['early-main']]);

    background.receive(backgroundMessages.shift()!);
    expect(background.state).toBe('adopted');
    expect(backgroundPatches).toEqual([['early-background']]);
  });

  test('replays an early event after node-id adoption', () => {
    const mainRecorder = new InitialFrameRecorder();
    const backgroundRecorder = new InitialFrameRecorder();
    recordFrame(mainRecorder, 1);
    recordFrame(backgroundRecorder, 1);
    const outgoing: Array<InitialFrameMessage> = [];
    const order: Array<string> = [];
    const main = new InitialFrameReconciler(
      'main',
      mainRecorder,
      { send: (message) => outgoing.push(message) },
      { adoptNodeIds: () => order.push('adopt') },
    );

    expect(main.deferEventUntilAdopted(() => order.push('event'), 'tap:bubble')).toBe(true);
    expect(main.status().deferredEventTypes).toEqual({ 'tap:bubble': 1 });
    main.receive({ type: 'manifest', manifest: backgroundRecorder.snapshot('session-1') });
    main.finalize('main-local');

    expect(order).toEqual(['adopt', 'event']);
    expect(outgoing[0]).toEqual({ type: 'ack', version: 1, session: 'session-1' });
    expect(main.deferEventUntilAdopted(() => order.push('unexpected'))).toBe(false);
    expect(main.status().deferredEventTypes).toEqual({});
  });

  test('fails closed on mismatch and sends a diagnostic NACK', () => {
    const mainRecorder = new InitialFrameRecorder();
    const backgroundRecorder = new InitialFrameRecorder();
    mainRecorder.record({ type: 'createElement', nodeId: 1, tag: 'view' });
    backgroundRecorder.record({ type: 'createElement', nodeId: 1, tag: 'text' });
    const outgoing: Array<InitialFrameMessage> = [];
    const errors: Array<string> = [];
    const main = new InitialFrameReconciler(
      'main',
      mainRecorder,
      { send: (message) => outgoing.push(message) },
      { reportError: (error) => errors.push(error) },
    );

    main.finalize('main-local');
    main.receive({ type: 'manifest', manifest: backgroundRecorder.snapshot('session-1') });

    expect(main.state).toBe('rejected');
    expect(outgoing[0]?.type).toBe('nack');
    expect((outgoing[0] as any).reason).toContain('operation 0 differs');
    expect(errors[0]).toContain('operation 0 differs');

    main.receive({ type: 'manifest', manifest: backgroundRecorder.snapshot('session-2') });
    expect(main.state).toBe('rejected');
    expect(outgoing[1]).toEqual({
      type: 'nack',
      version: 1,
      session: 'session-2',
      reason: expect.stringContaining('operation 0 differs'),
    });
  });

  test('bounds early events and remains rejected when a manifest later arrives', () => {
    const mainRecorder = new InitialFrameRecorder();
    const backgroundRecorder = new InitialFrameRecorder();
    recordFrame(mainRecorder, 1);
    recordFrame(backgroundRecorder, 1);
    const outgoing: Array<InitialFrameMessage> = [];
    const errors: Array<string> = [];
    const main = new InitialFrameReconciler(
      'main',
      mainRecorder,
      {
        send: (message) => outgoing.push(message),
      },
      {
        reportError: (error) => errors.push(error),
      },
    );

    for (let index = 0; index <= MAX_DEFERRED_INITIAL_FRAME_EVENTS; index++) {
      expect(main.deferEventUntilAdopted(() => {}, `tap-${index}`)).toBe(true);
    }
    expect(main.state).toBe('rejected');
    expect(main.status().deferredEvents).toBe(0);
    expect(errors[0]).toContain('deferred event queue exceeded 256 entries');

    main.finalize('main-local');
    main.receive({ type: 'manifest', manifest: backgroundRecorder.snapshot('session-1') });
    expect(main.state).toBe('rejected');
    expect(outgoing.at(-1)).toEqual({
      type: 'nack',
      version: 1,
      session: 'session-1',
      reason: 'deferred event queue exceeded 256 entries',
    });
  });

  test('ACKs a duplicate manifest but rejects a competing session', () => {
    const mainRecorder = new InitialFrameRecorder();
    const backgroundRecorder = new InitialFrameRecorder();
    recordFrame(mainRecorder, 1);
    recordFrame(backgroundRecorder, 1);
    const outgoing: Array<InitialFrameMessage> = [];
    const main = new InitialFrameReconciler('main', mainRecorder, {
      send: (message) => outgoing.push(message),
    });
    const first = backgroundRecorder.snapshot('session-1');

    main.finalize('main-local');
    main.receive({ type: 'manifest', manifest: first });
    main.receive({ type: 'manifest', manifest: first });
    main.receive({ type: 'manifest', manifest: { ...first, session: 'session-2' } });

    expect(outgoing.map((message) => message.type)).toEqual(['ack', 'ack', 'nack']);
    expect((outgoing[2] as any).reason).toBe('stale manifest after adoption');
  });

  test('keeps BTS adopted and rejected states terminal under reordered replies', () => {
    const rejectedRecorder = new InitialFrameRecorder();
    recordFrame(rejectedRecorder, 1);
    const rejected = new InitialFrameReconciler('background', rejectedRecorder, {
      send: () => {},
    });
    rejected.finalize('rejected-session');
    rejected.receive({
      type: 'nack',
      version: 1,
      session: 'rejected-session',
      reason: 'main rejected',
    });
    expect(rejected.state).toBe('rejected');
    rejected.receive({ type: 'ack', version: 1, session: 'rejected-session' });
    expect(rejected.state).toBe('rejected');

    const adoptedRecorder = new InitialFrameRecorder();
    recordFrame(adoptedRecorder, 1);
    const adopted = new InitialFrameReconciler('background', adoptedRecorder, {
      send: () => {},
    });
    adopted.finalize('adopted-session');
    adopted.receive({ type: 'ack', version: 1, session: 'adopted-session' });
    expect(adopted.state).toBe('adopted');
    adopted.receive({
      type: 'nack',
      version: 1,
      session: 'adopted-session',
      reason: 'late nack',
    });
    expect(adopted.state).toBe('adopted');
  });
});
