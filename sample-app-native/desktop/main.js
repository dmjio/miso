// Lynxtron main process for the miso sample. Pairs with ../Lynxtron.hs.
//
// Usage:
//   nix build ..#sample-app-native-lynxtron-bundle -o bundle   (from repo root)
//   cd sample-app-native/desktop && npm install && npm start
//
// Set MISO_BUNDLE to point at a different main.lynx.bundle.
const { app, LynxWindow, lynxBridge } = require('@lynx-js/lynxtron');
const os = require('node:os');
const path = require('node:path');
const fs = require('node:fs');

const bundle =
  process.env.MISO_BUNDLE ||
  path.resolve(__dirname, '../../result/main.lynx.bundle');

// card -> node, request/reply: Haskell `invokeNode "os-info"`.
lynxBridge.handle('os-info', () => ({
  platform: process.platform,
  arch: process.arch,
  hostname: os.hostname(),
  node: process.versions.node,
}));

async function createWindow() {
  if (!fs.existsSync(bundle)) {
    console.error(`bundle not found: ${bundle}\nbuild it with: nix build .#sample-app-native-lynxtron-bundle`);
    app.quit();
    return;
  }
  const win = new LynxWindow({
    width: 520,
    height: 720,
    title: 'miso × Lynxtron',
    lynxPreference: {
      // node -> card API surface: Haskell `callExposed`.
      preload: path.join(__dirname, 'preload.js'),
    },
  });

  // card -> node, fire-and-forget: Haskell `sendNode "log" {...}`.
  win.on('-lynx-message', (name, params) => {
    if (name === 'log') console.log('[card]', params.line);
    else console.log('[card message]', name, params);
  });

  win.show();
  await win.loadFile(bundle);

  // node -> card global event: Haskell `nodeEventSub "tick"`.
  let n = 0;
  const timer = setInterval(() => {
    if (win.isDestroyed()) return clearInterval(timer);
    win.sendGlobalEvent('tick', ++n);
  }, 1000);
}

app.whenReady().then(createWindow);
app.on('window-all-closed', () => app.quit());
