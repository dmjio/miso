import type { PATCH } from '../types';

export const INITIAL_FRAME_PROTOCOL_VERSION = 1;
export const INITIAL_FRAME_CHANNEL = 'Miso.initial-frame';

export type InitialFrameThread = 'background' | 'main';
export type InitialFrameState = 'recording' | 'awaiting-peer' | 'adopted' | 'rejected';

type CanonicalValue =
  | null
  | boolean
  | number
  | string
  | Array<CanonicalValue>
  | { [key: string]: CanonicalValue };

export type InitialFrameNode = {
  slot: number;
  nodeId: number;
  kind: 'root' | 'element' | 'elementNS' | 'text';
};

export type InitialFrameOperation = {
  type: PATCH['type'];
  [key: string]: CanonicalValue;
};

export type InitialFrameManifest = {
  version: typeof INITIAL_FRAME_PROTOCOL_VERSION;
  session: string;
  nodes: Array<InitialFrameNode>;
  operations: Array<InitialFrameOperation>;
};

export type InitialFrameMessage =
  | { type: 'manifest'; manifest: InitialFrameManifest }
  | { type: 'ack'; version: number; session: string }
  | { type: 'nack'; version: number; session: string; reason: string };

export type InitialFrameComparison =
  | { ok: true; nodeIds: Map<number, number> }
  | { ok: false; reason: string };

function canonicalValue(value: any, seen: Array<object> = []): CanonicalValue {
  if (value === null || typeof value === 'string' || typeof value === 'boolean') return value;
  if (typeof value === 'number') {
    if (Number.isNaN(value)) return { '$ifr': 'NaN' };
    if (value === Infinity) return { '$ifr': 'Infinity' };
    if (value === -Infinity) return { '$ifr': '-Infinity' };
    if (Object.is(value, -0)) return { '$ifr': '-0' };
    return value;
  }
  if (typeof value === 'undefined') return { '$ifr': 'undefined' };
  if (typeof value === 'bigint') return { '$ifr': 'bigint', value: String(value) };
  if (typeof value === 'function') return { '$ifr': 'function' };
  if (typeof value === 'symbol') return { '$ifr': 'symbol', value: String(value) };
  if (typeof value !== 'object') return { '$ifr': typeof value, value: String(value) };

  if (seen.includes(value)) return { '$ifr': 'cycle' };
  seen.push(value);
  if (Array.isArray(value)) {
    const result = value.map((item) => canonicalValue(item, seen));
    seen.pop();
    return result;
  }

  const result: Record<string, CanonicalValue> = {};
  for (const key of Object.keys(value).sort()) result[key] = canonicalValue(value[key], seen);
  seen.pop();
  return result;
}

function canonicalJSON(value: unknown): string {
  return JSON.stringify(canonicalValue(value));
}

/**
 * Records the initial draw as operations over creation slots instead of local
 * node ids. Slot 0 is the page root; every create operation allocates the next
 * slot. This makes structural equality independent of whichever ids the two
 * Lynx threads happened to allocate locally.
 */
export class InitialFrameRecorder {
  private readonly slots = new Map<number, number>([[0, 0]]);
  private readonly nodes: Array<InitialFrameNode> = [{ slot: 0, nodeId: 0, kind: 'root' }];
  private readonly operations: Array<InitialFrameOperation> = [];

  record(patch: PATCH): void {
    if (patch.type === 'flush') return;

    const operation: Record<string, CanonicalValue> = { type: patch.type };
    switch (patch.type) {
      case 'createElement':
        operation.node = this.createSlot(patch.nodeId, 'element');
        operation.tag = patch.tag;
        break;
      case 'createElementNS':
        operation.node = this.createSlot(patch.nodeId, 'elementNS');
        operation.namespace = patch.namespace;
        operation.tag = patch.tag;
        break;
      case 'createTextNode':
        operation.node = this.createSlot(patch.nodeId, 'text');
        operation.text = patch.text;
        break;
      case 'appendChild':
      case 'removeChild':
        operation.parent = this.slotFor(patch.parent);
        operation.child = this.slotFor(patch.child);
        break;
      case 'replaceChild':
        operation.parent = this.slotFor(patch.parent);
        operation.current = this.slotFor(patch.current);
        operation.new = this.slotFor(patch.new);
        break;
      case 'insertBefore':
        operation.parent = this.slotFor(patch.parent);
        operation.node = this.slotFor(patch.node);
        operation.child = this.slotFor(patch.child);
        break;
      case 'swapDOMRefs':
        operation.parent = this.slotFor(patch.parent);
        operation.nodeA = this.slotFor(patch.nodeA);
        operation.nodeB = this.slotFor(patch.nodeB);
        break;
      case 'setAttribute':
        operation.node = this.slotFor(patch.nodeId);
        operation.key = patch.key;
        operation.value = canonicalValue(patch.value);
        break;
      case 'setAttributeNS':
        operation.node = this.slotFor(patch.nodeId);
        operation.namespace = patch.namespace;
        operation.key = patch.key;
        operation.value = canonicalValue(patch.value);
        break;
      case 'removeAttribute':
      case 'addClass':
      case 'removeClass':
        operation.node = this.slotFor(patch.nodeId);
        operation.key = patch.key;
        break;
      case 'setTextContent':
        operation.node = this.slotFor(patch.nodeId);
        operation.text = patch.text;
        break;
      case 'setInlineStyle':
        operation.node = this.slotFor(patch.nodeId);
        operation.current = canonicalValue(patch.current);
        operation.new = canonicalValue(patch.new);
        break;
      case 'addEvent':
        operation.node = this.slotFor(patch.nodeId);
        operation.name = patch.name;
        operation.capture = patch.capture;
        operation.staticKey = canonicalValue(patch.staticKey);
        operation.componentId = canonicalValue(patch.componentId);
        operation.options = canonicalValue(patch.options);
        operation.direct = canonicalValue(patch.direct);
        break;
      case 'removeEvent':
        operation.node = this.slotFor(patch.nodeId);
        operation.name = patch.name;
        operation.capture = patch.capture;
        break;
    }
    this.operations.push(operation as InitialFrameOperation);
  }

  snapshot(session: string): InitialFrameManifest {
    return {
      version: INITIAL_FRAME_PROTOCOL_VERSION,
      session,
      nodes: this.nodes.map((node) => ({ ...node })),
      operations: this.operations.map((operation) => ({ ...operation })),
    };
  }

  private createSlot(nodeId: number, kind: InitialFrameNode['kind']): number {
    if (this.slots.has(nodeId)) throw new Error(`initial-frame duplicate nodeId ${nodeId}`);
    const slot = this.nodes.length;
    this.slots.set(nodeId, slot);
    this.nodes.push({ slot, nodeId, kind });
    return slot;
  }

  private slotFor(nodeId: number): number {
    const slot = this.slots.get(nodeId);
    if (slot === undefined) throw new Error(`initial-frame reference to unknown nodeId ${nodeId}`);
    return slot;
  }
}

export function compareInitialFrames(
  local: InitialFrameManifest,
  authoritative: InitialFrameManifest,
): InitialFrameComparison {
  if (local.version !== authoritative.version) {
    return { ok: false, reason: `protocol version ${local.version} != ${authoritative.version}` };
  }
  if (local.nodes.length !== authoritative.nodes.length) {
    return {
      ok: false,
      reason: `node count ${local.nodes.length} != ${authoritative.nodes.length}`,
    };
  }
  if (local.operations.length !== authoritative.operations.length) {
    return {
      ok: false,
      reason: `operation count ${local.operations.length} != ${authoritative.operations.length}`,
    };
  }

  for (let index = 0; index < local.operations.length; index++) {
    const left = canonicalJSON(local.operations[index]);
    const right = canonicalJSON(authoritative.operations[index]);
    if (left !== right) {
      return {
        ok: false,
        reason: `operation ${index} differs: main=${left} background=${right}`,
      };
    }
  }

  const nodeIds = new Map<number, number>();
  for (let slot = 0; slot < local.nodes.length; slot++) {
    const localNode = local.nodes[slot];
    const remoteNode = authoritative.nodes[slot];
    if (localNode.slot !== slot || remoteNode.slot !== slot || localNode.kind !== remoteNode.kind) {
      return {
        ok: false,
        reason: `node slot ${slot} differs: main=${canonicalJSON(localNode)} background=${canonicalJSON(remoteNode)}`,
      };
    }
    nodeIds.set(localNode.nodeId, remoteNode.nodeId);
  }
  return { ok: true, nodeIds };
}

export type InitialFrameTransport = {
  send: (message: InitialFrameMessage) => void;
};

export type InitialFrameHooks<PatchBatch> = {
  adoptNodeIds?: (nodeIds: Map<number, number>) => void;
  deliverPatches?: (patches: PatchBatch) => void;
  reportError?: (message: string) => void;
  scheduleRetry?: (callback: () => void, delayMs: number) => unknown;
  cancelRetry?: (token: unknown) => void;
};

/**
 * Thread-neutral IFR state machine. The BTS publishes the authoritative
 * manifest and holds incremental patches until ACK. The MTS compares, adopts
 * ids atomically, ACKs, then replays any early patches/events in arrival order.
 */
export class InitialFrameReconciler<PatchBatch = Array<PATCH>> {
  state: InitialFrameState = 'recording';
  private localManifest?: InitialFrameManifest;
  private peerManifest?: InitialFrameManifest;
  private session?: string;
  private retryToken?: unknown;
  private retryCount = 0;
  private rejectionReason?: string;
  private readonly pendingPatches: Array<PatchBatch> = [];
  private readonly deferredEvents: Array<() => void> = [];

  constructor(
    readonly thread: InitialFrameThread,
    readonly recorder: InitialFrameRecorder,
    private readonly transport: InitialFrameTransport,
    private readonly hooks: InitialFrameHooks<PatchBatch> = {},
  ) {}

  finalize(session = this.makeSession()): void {
    if (this.localManifest) return;
    this.session = session;
    this.localManifest = this.recorder.snapshot(session);
    this.state = 'awaiting-peer';
    if (this.thread === 'background') {
      this.publishManifest();
    } else {
      this.tryAdopt();
    }
  }

  receive(message: InitialFrameMessage): void {
    if (message.type !== 'manifest' && message.version !== INITIAL_FRAME_PROTOCOL_VERSION) {
      this.reject(`unsupported peer protocol version ${message.version}`, message.session);
      return;
    }

    if (message.type === 'manifest') {
      if (this.thread !== 'main') return;
      if (message.manifest.version !== INITIAL_FRAME_PROTOCOL_VERSION) {
        this.reject(
          `unsupported manifest version ${message.manifest.version}`,
          message.manifest.session,
        );
        return;
      }
      if (this.state === 'adopted') {
        if (message.manifest.session === this.peerManifest?.session)
          this.sendAck(message.manifest.session);
        else this.sendNack(message.manifest.session, 'stale manifest after adoption');
        return;
      }
      if (this.peerManifest && this.peerManifest.session !== message.manifest.session) {
        this.sendNack(message.manifest.session, 'competing manifest while adoption is pending');
        return;
      }
      this.peerManifest = message.manifest;
      this.tryAdopt();
      return;
    }

    if (this.thread !== 'background' || message.session !== this.session) return;
    if (message.type === 'ack') {
      this.cancelManifestRetry();
      this.state = 'adopted';
      this.drainPatches();
    } else {
      this.reject(`main thread rejected initial frame: ${message.reason}`, message.session);
    }
  }

  sendOrQueuePatches(patches: PatchBatch): void {
    if (this.state === 'adopted') this.hooks.deliverPatches?.(patches);
    else if (this.state !== 'rejected') this.pendingPatches.push(patches);
  }

  receiveOrQueuePatches(patches: PatchBatch): void {
    if (this.state === 'adopted') this.hooks.deliverPatches?.(patches);
    else if (this.state !== 'rejected') this.pendingPatches.push(patches);
  }

  setPatchDelivery(deliverPatches: (patches: PatchBatch) => void): void {
    this.hooks.deliverPatches = deliverPatches;
    if (this.state === 'adopted') this.drainPatches();
  }

  status(): Record<string, string | number | undefined> {
    return {
      thread: this.thread,
      state: this.state,
      session: this.session,
      localNodes: this.localManifest?.nodes.length,
      localOperations: this.localManifest?.operations.length,
      localManifestBytes: this.localManifest
        ? JSON.stringify(this.localManifest).length
        : undefined,
      peerNodes: this.peerManifest?.nodes.length,
      peerOperations: this.peerManifest?.operations.length,
      queuedPatchBatches: this.pendingPatches.length,
      deferredEvents: this.deferredEvents.length,
      retries: this.retryCount,
      rejectionReason: this.rejectionReason,
    };
  }

  /** Returns true when the caller must stop; the callback will replay after adoption. */
  deferEventUntilAdopted(callback: () => void): boolean {
    if (this.state === 'adopted') return false;
    if (this.state !== 'rejected') this.deferredEvents.push(callback);
    return true;
  }

  private tryAdopt(): void {
    if (this.thread !== 'main' || !this.localManifest || !this.peerManifest) return;
    const result = compareInitialFrames(this.localManifest, this.peerManifest);
    if (result.ok === false) {
      this.reject(result.reason, this.peerManifest.session);
      return;
    }
    try {
      this.hooks.adoptNodeIds?.(result.nodeIds);
    } catch (error) {
      this.reject(`node-id adoption threw: ${String(error)}`, this.peerManifest.session);
      return;
    }
    this.state = 'adopted';
    this.sendAck(this.peerManifest.session);
    this.drainPatches();
    const events = this.deferredEvents.splice(0);
    for (const replay of events) replay();
  }

  private publishManifest(): void {
    if (!this.localManifest || this.state === 'adopted' || this.state === 'rejected') return;
    if (this.retryCount >= 20) {
      this.reject(
        'initial-frame manifest was not acknowledged after 20 attempts',
        this.localManifest.session,
      );
      return;
    }
    this.retryCount++;
    this.transport.send({ type: 'manifest', manifest: this.localManifest });
    if (!this.hooks.scheduleRetry) return;
    this.retryToken = this.hooks.scheduleRetry(() => this.publishManifest(), 50);
  }

  private sendAck(session: string): void {
    this.transport.send({ type: 'ack', version: INITIAL_FRAME_PROTOCOL_VERSION, session });
  }

  private sendNack(session: string, reason: string): void {
    this.transport.send({
      type: 'nack',
      version: INITIAL_FRAME_PROTOCOL_VERSION,
      session,
      reason,
    });
  }

  private reject(reason: string, session: string): void {
    this.cancelManifestRetry();
    this.state = 'rejected';
    this.rejectionReason = reason;
    this.pendingPatches.length = 0;
    this.deferredEvents.length = 0;
    if (this.thread === 'main') this.sendNack(session, reason);
    this.hooks.reportError?.(`[miso IFR] ${reason}`);
  }

  private drainPatches(): void {
    const pending = this.pendingPatches.splice(0);
    for (const patches of pending) this.hooks.deliverPatches?.(patches);
  }

  private cancelManifestRetry(): void {
    if (this.retryToken !== undefined) this.hooks.cancelRetry?.(this.retryToken);
    this.retryToken = undefined;
  }

  private makeSession(): string {
    return `ifr-${Date.now()}-${Math.random().toString(16).slice(2)}`;
  }
}

export const initialFrameRecorder = new InitialFrameRecorder();
