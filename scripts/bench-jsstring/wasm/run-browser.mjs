// Runs the MicroHs build (app.js from build-mhs.sh) in headless Chromium and
// prints its PROP| and BENCH| lines. MicroHs is much slower than GHC, so by
// default it runs 300-character inputs (the old module's lazy accumulators
// overflow MicroHs's stack on a few KB), 300 cases per property and no whole
// workloads. Usage: node run-browser.mjs app.js [sizes, comma-separated] [cases]
// Needs playwright next to it:
//   npm install --no-save --prefix scripts/bench-jsstring/wasm playwright@1.48.2
//   npx --prefix scripts/bench-jsstring/wasm playwright install --with-deps chromium
import { chromium } from 'playwright';
import { writeFileSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { pathToFileURL } from 'node:url';

const [app, sizes = '300', cases = '300'] = process.argv.slice(2);
const html = resolve(dirname(app), 'bench.html');
writeFileSync(html, `<!DOCTYPE html><html><head><meta charset="utf-8"></head><body>
<script>globalThis.benchSizes = ${JSON.stringify(String(sizes))}; globalThis.benchCases = ${Number(cases)}; globalThis.benchWhole = false;</script>
<script src="app.js"></script></body></html>`);

const browser = await chromium.launch();
const page = await browser.newPage();
console.log('RUNNER|start|mhs');
const done = new Promise((ok, fail) => {
  page.on('console', msg => {
    const text = msg.text();
    if (msg.type() === 'error') console.error(text); else console.log(text);
    if (text === 'DONE') ok();
    if (text.includes('uncaught exception')) fail(new Error(text));
  });
  page.on('pageerror', err => { console.error('pageerror: ' + err.message); fail(err); });
});
await page.goto(pathToFileURL(html).href);
const limit = new Promise((_, fail) => setTimeout(() => fail(new Error('timed out')), 100 * 60 * 1000));
try { await Promise.race([done, limit]); console.log('RUNNER|finished'); }
catch (e) { console.error('RUNNER|failed|' + e.message); process.exitCode = 1; }
// A page that died can keep the browser from closing: do not wait for it.
await Promise.race([browser.close(), new Promise(ok => setTimeout(ok, 5000))]);
process.exit(process.exitCode ?? 0);
