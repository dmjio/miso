// Runs in the Lynx background thread with Node access. Everything passed to
// `exposeInLynxBTS` appears as `NativeModules.nodejs.exposed.<name>` and is
// reachable from Haskell via `Miso.Native.Lynxtron.callExposed`.
const { contextBridge } = require('@lynx-js/lynxtron/context-bridge');
const fs = require('node:fs/promises');

contextBridge.exposeInLynxBTS({
  // async — callExposed awaits the Promise
  greet: async (who) => `hello, ${who}, from node ${process.versions.node}`,

  // nested object — addressed as "fileApi.exists"
  fileApi: {
    exists: async (p) => {
      try { await fs.access(p); return true; } catch { return false; }
    },
  },
});
