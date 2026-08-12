/* Keep the MTS allocator in lockstep with node ids created by BTS patches.
   Initial draw advances both allocators independently, but after that only BTS
   creates VDOM nodes. If MTS merely registers those ids in runtime.nodes, its
   allocator stays behind and can later reuse a live id. */
export function observePatchedNodeId (nodeId : number) {
  globalThis['nodeId'] = Math.max(globalThis['nodeId'], nodeId + 1);
}
