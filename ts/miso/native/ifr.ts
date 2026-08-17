import type { PATCH } from '../types';

export const INITIAL_FRAME_PROTOCOL_VERSION = 1;
export const INITIAL_FRAME_CHANNEL = 'Miso.initial-frame';
export const MAX_DEFERRED_INITIAL_FRAME_EVENTS = 256;

export type InitialFrameThread = 'background' | 'main';
export type InitialFrameState =
  | 'recording'
  | 'awaiting-peer'
  | 'recovering'
  | 'adopted'
  | 'rejected';

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

/**
 * Production handshake payload. The authoritative node-id vector is the only
 * O(tree size) field: slot-normalized adoption needs it, while the much larger
 * operation stream is represented by a rolling hash and two counts.
 */
export type InitialFrameDigest = {
  version: typeof INITIAL_FRAME_PROTOCOL_VERSION;
  session: string;
  nodeCount: number;
  operationCount: number;
  operationHash: string;
  nodeIds: Array<number>;
};

export type InitialFrameMessage =
  | { type: 'digest'; digest: InitialFrameDigest }
  | { type: 'manifest'; manifest: InitialFrameManifest }
  | { type: 'diagnostic'; manifest: InitialFrameManifest }
  | { type: 'ack'; version: number; session: string }
  | { type: 'nack'; version: number; session: string; reason: string }
  | { type: 'fallback-start'; version: number; session: string; reason: string }
  | { type: 'fallback-complete'; version: number; session: string };

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

// Two independently seeded 32-bit multiplicative lanes make a compact rolling
// fingerprint. Length-prefixing each canonical operation keeps boundaries
// unambiguous without retaining the canonical operation stream in production.
const HASH_OFFSET_A = 0x811c9dc5;
const HASH_OFFSET_B = 0x9e3779b9;
const HASH_PRIME_A = 0x01000193;
const HASH_PRIME_B = 0x85ebca6b;

function updateHash(hash: number, value: string, prime: number): number {
  for (let index = 0; index < value.length; index++) {
    const code = value.charCodeAt(index);
    hash = Math.imul(hash ^ (code & 0xff), prime);
    hash = Math.imul(hash ^ (code >>> 8), prime);
  }
  return hash >>> 0;
}

function hex32(value: number): string {
  return value.toString(16).padStart(8, '0');
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
  private readonly rawPatches: Array<PATCH> = [];
  private readonly retainedOperations?: Array<InitialFrameOperation>;
  private operationCount = 0;
  private hashA = HASH_OFFSET_A;
  private hashB = HASH_OFFSET_B;

  constructor(retainOperations = false) {
    if (retainOperations) this.retainedOperations = [];
  }

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
    const normalized = operation as InitialFrameOperation;
    const serialized = canonicalJSON(normalized);
    const framed = `${serialized.length}:${serialized}`;
    this.hashA = updateHash(this.hashA, framed, HASH_PRIME_A);
    this.hashB = updateHash(this.hashB, framed, HASH_PRIME_B);
    this.operationCount++;
    this.retainedOperations?.push(normalized);
    // Keep ordinary patches for the safe BTS-authoritative repaint path. The
    // canonicalized operation stream itself is retained only by an on-demand
    // debug recorder (snapshot()), not on the production startup hot path.
    this.rawPatches.push({ ...patch } as PATCH);
  }

  digest(session: string): InitialFrameDigest {
    return {
      version: INITIAL_FRAME_PROTOCOL_VERSION,
      session,
      nodeCount: this.nodes.length,
      operationCount: this.operationCount,
      operationHash: `${hex32(this.hashA)}${hex32(this.hashB)}`,
      nodeIds: this.nodes.map(({ nodeId }) => nodeId),
    };
  }

  snapshot(session: string): InitialFrameManifest {
    if (!this.retainedOperations) {
      const diagnostic = new InitialFrameRecorder(true);
      for (const patch of this.rawPatches) diagnostic.record(patch);
      return diagnostic.snapshot(session);
    }
    return {
      version: INITIAL_FRAME_PROTOCOL_VERSION,
      session,
      nodes: this.nodes.map((node) => ({ ...node })),
      operations: this.retainedOperations.map((operation) => ({ ...operation })),
    };
  }

  /** Full BTS tree as ordinary patches, used only after a NACK. */
  fullTreePatches(): Array<PATCH> {
    return this.rawPatches.map((patch) => ({ ...patch }) as PATCH);
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

export function compareInitialFrameDigests(
  local: InitialFrameDigest,
  authoritative: InitialFrameDigest,
): InitialFrameComparison {
  if (local.version !== authoritative.version) {
    return { ok: false, reason: `protocol version ${local.version} != ${authoritative.version}` };
  }
  if (local.nodeCount !== authoritative.nodeCount) {
    return { ok: false, reason: `node count ${local.nodeCount} != ${authoritative.nodeCount}` };
  }
  if (local.operationCount !== authoritative.operationCount) {
    return {
      ok: false,
      reason: `operation count ${local.operationCount} != ${authoritative.operationCount}`,
    };
  }
  if (local.operationHash !== authoritative.operationHash) {
    return {
      ok: false,
      reason: `operation hash ${local.operationHash} != ${authoritative.operationHash}`,
    };
  }
  if (local.nodeIds.length !== local.nodeCount) {
    return {
      ok: false,
      reason: `local node-id count ${local.nodeIds.length} != node count ${local.nodeCount}`,
    };
  }
  if (authoritative.nodeIds.length !== authoritative.nodeCount) {
    return {
      ok: false,
      reason: `authoritative node-id count ${authoritative.nodeIds.length} != node count ${authoritative.nodeCount}`,
    };
  }

  const nodeIds = new Map<number, number>();
  const authoritativeIds = new Set<number>();
  for (let slot = 0; slot < local.nodeCount; slot++) {
    const localNodeId = local.nodeIds[slot];
    const authoritativeNodeId = authoritative.nodeIds[slot];
    if (nodeIds.has(localNodeId)) {
      return { ok: false, reason: `duplicate local nodeId ${localNodeId} at slot ${slot}` };
    }
    if (authoritativeIds.has(authoritativeNodeId)) {
      return {
        ok: false,
        reason: `duplicate authoritative nodeId ${authoritativeNodeId} at slot ${slot}`,
      };
    }
    nodeIds.set(localNodeId, authoritativeNodeId);
    authoritativeIds.add(authoritativeNodeId);
  }
  return { ok: true, nodeIds };
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
  fallbackPatches?: () => PatchBatch;
  prepareFallback?: () => void;
  debugManifest?: boolean;
  reportError?: (message: string) => void;
  scheduleRetry?: (callback: () => void, delayMs: number) => unknown;
  cancelRetry?: (token: unknown) => void;
};

/**
 * Thread-neutral IFR state machine. The BTS publishes the authoritative
 * digest and holds incremental patches until ACK. The MTS compares, adopts ids
 * atomically, ACKs, then replays any early patches/events in arrival order. A
 * mismatch degrades to a BTS-authoritative full-tree repaint instead of
 * freezing the application.
 */
export class InitialFrameReconciler<PatchBatch = Array<PATCH>> {
  state: InitialFrameState = 'recording';
  private localDigest?: InitialFrameDigest;
  private peerDigest?: InitialFrameDigest;
  private localManifest?: InitialFrameManifest;
  private peerManifest?: InitialFrameManifest;
  private session?: string;
  private retryToken?: unknown;
  private retryCount = 0;
  private rejectionReason?: string;
  private recoveryReason?: string;
  private fallbackPrepared = false;
  private readonly pendingPatches: Array<PatchBatch> = [];
  private readonly deferredEvents: Array<{ label: string; replay: () => void }> = [];

  constructor(
    readonly thread: InitialFrameThread,
    readonly recorder: InitialFrameRecorder,
    private readonly transport: InitialFrameTransport,
    private readonly hooks: InitialFrameHooks<PatchBatch> = {},
  ) {}

  finalize(session = this.makeSession()): void {
    if (this.localDigest || this.state === 'adopted' || this.state === 'rejected') return;
    this.session = session;
    this.localDigest = this.recorder.digest(session);
    if (this.hooks.debugManifest) this.localManifest = this.recorder.snapshot(session);
    this.state = 'awaiting-peer';
    if (this.thread === 'background') {
      this.publishInitialFrame();
    } else {
      this.tryAdopt();
    }
  }

  receive(message: InitialFrameMessage): void {
    // Adoption/rejection are terminal. A delayed ACK/NACK must not resurrect a
    // rejected BTS or roll an adopted BTS back after messages are reordered.
    // A rejected MTS still answers a retried frame with its original NACK so
    // the BTS can converge on the same terminal result.
    if (this.state === 'rejected') {
      const session = this.frameSession(message);
      if (session && this.thread === 'main') {
        this.sendNack(session, this.rejectionReason ?? 'initial frame was already rejected');
      }
      return;
    }

    if (message.type === 'digest') {
      this.receiveFrame('digest', message.digest);
      return;
    }
    if (message.type === 'manifest') {
      this.receiveFrame('manifest', message.manifest);
      return;
    }
    if (message.type === 'diagnostic') {
      this.receiveDiagnostic(message.manifest);
      return;
    }

    if (message.version !== INITIAL_FRAME_PROTOCOL_VERSION) {
      this.reject(`unsupported peer protocol version ${message.version}`, message.session);
      return;
    }
    if (message.type === 'fallback-start') {
      this.prepareAuthoritativeFallback(message.session, message.reason);
      return;
    }
    if (message.type === 'fallback-complete') {
      this.completeAuthoritativeFallback(message.session);
      return;
    }

    if (
      this.thread !== 'background' ||
      message.session !== this.session ||
      this.state === 'adopted' ||
      this.state === 'recovering'
    )
      return;
    if (message.type === 'ack') {
      this.cancelManifestRetry();
      this.state = 'adopted';
      this.drainPatches();
    } else {
      this.startAuthoritativeFallback(message.reason, message.session);
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

  status(): Record<string, unknown> {
    const deferredEventTypes: Record<string, number> = {};
    for (const { label } of this.deferredEvents) {
      deferredEventTypes[label] = (deferredEventTypes[label] ?? 0) + 1;
    }
    return {
      thread: this.thread,
      state: this.state,
      session: this.session,
      handshakeMode: this.hooks.debugManifest ? 'manifest' : 'digest',
      localNodes: this.localDigest?.nodeCount,
      localOperations: this.localDigest?.operationCount,
      localDigestBytes: this.localDigest ? JSON.stringify(this.localDigest).length : undefined,
      localManifestBytes: this.localManifest
        ? JSON.stringify(this.localManifest).length
        : undefined,
      peerNodes: this.peerDigest?.nodeCount ?? this.peerManifest?.nodes.length,
      peerOperations: this.peerDigest?.operationCount ?? this.peerManifest?.operations.length,
      queuedPatchBatches: this.pendingPatches.length,
      deferredEvents: this.deferredEvents.length,
      deferredEventTypes,
      retries: this.retryCount,
      recoveryReason: this.recoveryReason,
      rejectionReason: this.rejectionReason,
    };
  }

  /** Returns true when the caller must stop; the callback will replay after adoption. */
  deferEventUntilAdopted(callback: () => void, label = 'unknown'): boolean {
    if (this.state === 'adopted') return false;
    if (this.state !== 'rejected') {
      if (this.deferredEvents.length >= MAX_DEFERRED_INITIAL_FRAME_EVENTS) {
        this.reject(
          `deferred event queue exceeded ${MAX_DEFERRED_INITIAL_FRAME_EVENTS} entries`,
          this.authoritativeSession() ?? this.session ?? 'unknown',
        );
      } else {
        this.deferredEvents.push({ label, replay: callback });
      }
    }
    return true;
  }

  private tryAdopt(): void {
    if (
      this.thread !== 'main' ||
      this.state === 'adopted' ||
      this.state === 'recovering' ||
      this.state === 'rejected' ||
      !this.localDigest ||
      (!this.peerDigest && !this.peerManifest)
    )
      return;
    const session = this.authoritativeSession()!;
    const result = this.peerManifest
      ? compareInitialFrames(
          this.localManifest ?? this.recorder.snapshot(this.localDigest.session),
          this.peerManifest,
        )
      : compareInitialFrameDigests(this.localDigest, this.peerDigest!);
    if (result.ok === false) {
      this.requestAuthoritativeFallback(result.reason, session);
      return;
    }
    try {
      this.hooks.adoptNodeIds?.(result.nodeIds);
    } catch (error) {
      this.requestAuthoritativeFallback(`node-id adoption threw: ${String(error)}`, session);
      return;
    }
    // Once the manifests agree, the BTS session is authoritative for both
    // threads. Keeping the MTS-local nonce here made diagnostics look like two
    // different reconciliations even though adoption had succeeded.
    this.session = session;
    this.state = 'adopted';
    this.sendAck(session);
    this.drainPatches();
    this.replayEvents();
  }

  private publishInitialFrame(): void {
    if (!this.localDigest || this.state !== 'awaiting-peer') return;
    if (this.retryCount >= 20) {
      this.reject(
        'initial-frame handshake was not acknowledged after 20 attempts',
        this.localDigest.session,
      );
      return;
    }
    this.retryCount++;
    if (this.hooks.debugManifest) {
      this.localManifest ??= this.recorder.snapshot(this.localDigest.session);
      this.transport.send({ type: 'manifest', manifest: this.localManifest });
    } else {
      this.transport.send({ type: 'digest', digest: this.localDigest });
    }
    if (!this.hooks.scheduleRetry) return;
    this.retryToken = this.hooks.scheduleRetry(() => this.publishInitialFrame(), 50);
  }

  private receiveFrame(
    kind: 'digest' | 'manifest',
    frame: InitialFrameDigest | InitialFrameManifest,
  ): void {
    if (this.thread !== 'main') return;
    if (frame.version !== INITIAL_FRAME_PROTOCOL_VERSION) {
      this.reject(`unsupported ${kind} version ${frame.version}`, frame.session);
      return;
    }
    if (this.state === 'adopted') {
      if (frame.session === this.authoritativeSession()) this.sendAck(frame.session);
      else this.sendNack(frame.session, `stale ${kind} after adoption`);
      return;
    }
    if (this.state === 'recovering') {
      this.sendNack(frame.session, this.recoveryReason ?? 'authoritative repaint requested');
      return;
    }
    const peerSession = this.authoritativeSession();
    if (peerSession && peerSession !== frame.session) {
      this.sendNack(frame.session, `competing ${kind} while adoption is pending`);
      return;
    }
    if (kind === 'digest') this.peerDigest = frame as InitialFrameDigest;
    else this.peerManifest = frame as InitialFrameManifest;
    this.tryAdopt();
  }

  private receiveDiagnostic(manifest: InitialFrameManifest): void {
    if (
      this.thread !== 'main' ||
      manifest.version !== INITIAL_FRAME_PROTOCOL_VERSION ||
      manifest.session !== this.authoritativeSession()
    )
      return;
    const localSession = this.localDigest?.session ?? this.session ?? 'diagnostic';
    const result = compareInitialFrames(this.recorder.snapshot(localSession), manifest);
    if (result.ok === false) {
      this.hooks.reportError?.(`[miso IFR diagnostic] ${result.reason}`);
    } else {
      this.hooks.reportError?.(
        '[miso IFR diagnostic] compact digest differed but full manifests matched',
      );
    }
  }

  private requestAuthoritativeFallback(reason: string, session: string): void {
    if (this.state === 'recovering' || this.state === 'rejected') return;
    this.state = 'recovering';
    this.recoveryReason = reason;
    // Events targeted the discarded MTS-local tree. Incremental patch batches
    // are re-sent by the BTS after the full-tree batch, so discard any early
    // copies before requesting recovery.
    this.pendingPatches.length = 0;
    this.deferredEvents.length = 0;
    this.hooks.reportError?.(`[miso IFR] ${reason}; requesting BTS-authoritative repaint`);
    this.sendNack(session, reason);
  }

  private startAuthoritativeFallback(reason: string, session: string): void {
    this.cancelManifestRetry();
    const patches = this.hooks.fallbackPatches?.();
    if (patches === undefined || !this.hooks.deliverPatches) {
      this.reject(`initial-frame fallback is unavailable after NACK: ${reason}`, session);
      return;
    }
    this.state = 'recovering';
    this.recoveryReason = reason;
    this.hooks.reportError?.(`[miso IFR] ${reason}; repainting from BTS patches`);
    // Full manifests are intentionally off the production startup path. A NACK
    // is the one production case that pays this cost, solely for diagnosis.
    this.transport.send({ type: 'diagnostic', manifest: this.recorder.snapshot(session) });
    this.transport.send({
      type: 'fallback-start',
      version: INITIAL_FRAME_PROTOCOL_VERSION,
      session,
      reason,
    });
    try {
      this.hooks.deliverPatches(patches);
    } catch (error) {
      this.reject(`initial-frame fallback delivery threw: ${String(error)}`, session);
      return;
    }
    this.transport.send({
      type: 'fallback-complete',
      version: INITIAL_FRAME_PROTOCOL_VERSION,
      session,
    });
    this.state = 'adopted';
    this.drainPatches();
  }

  private prepareAuthoritativeFallback(session: string, reason: string): void {
    if (
      this.thread !== 'main' ||
      this.state !== 'recovering' ||
      session !== this.authoritativeSession() ||
      this.fallbackPrepared
    )
      return;
    try {
      this.hooks.prepareFallback?.();
      this.fallbackPrepared = true;
      this.recoveryReason = reason;
    } catch (error) {
      this.reject(`initial-frame fallback preparation threw: ${String(error)}`, session);
    }
  }

  private completeAuthoritativeFallback(session: string): void {
    if (
      this.thread !== 'main' ||
      this.state !== 'recovering' ||
      !this.fallbackPrepared ||
      session !== this.authoritativeSession()
    )
      return;
    this.session = session;
    this.state = 'adopted';
    this.drainPatches();
    this.replayEvents();
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

  private authoritativeSession(): string | undefined {
    return this.peerDigest?.session ?? this.peerManifest?.session;
  }

  private frameSession(message: InitialFrameMessage): string | undefined {
    if (message.type === 'digest') return message.digest.session;
    if (message.type === 'manifest') return message.manifest.session;
    return undefined;
  }

  private drainPatches(): void {
    const pending = this.pendingPatches.splice(0);
    for (const patches of pending) this.hooks.deliverPatches?.(patches);
  }

  private replayEvents(): void {
    const events = this.deferredEvents.splice(0);
    for (const { replay } of events) replay();
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
