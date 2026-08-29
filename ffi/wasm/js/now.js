// High-resolution timestamp where one exists, wall clock where it does not.
// See ffi/wasm/Miso/DSL/FFI.hs's now_ffi.
function misoNow() {
  return (typeof performance !== 'undefined' && performance && typeof performance.now === 'function')
    ? performance.now()
    : Date.now();
}
