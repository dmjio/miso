// Measure the canvas benchmark's frame rate with Playwright.
//
//   node bench.mjs [url] [seconds] [rects]
//
// Loads the page with ?rects=N, then
// collects the "fps: N ms/frame: T rects: M" console lines the app logs once a second
// and prints their median.  Serve public-mhs first (make serve-mhs).
import { chromium } from 'playwright';

const url = process.argv[2] ?? 'http://localhost:8123/';
const seconds = Number(process.argv[3] ?? 12);
const rects = Number(process.argv[4] ?? 1000);

// HEADED=1 runs a visible Chromium, which composites with the GPU; headless
// Chromium rasterises the canvas in software and reports lower frame rates.
const browser = await chromium.launch({
  headless: !process.env.HEADED,
});
const page = await browser.newPage();
const samples = [];
page.on('console', (msg) => {
  const m = /^fps: ([\d.]+) ms\/frame: (\d+) rects: (\d+)/.exec(msg.text());
  if (m) samples.push({ fps: Number(m[1]), ms: Number(m[2]), rects: Number(m[3]) });
});
page.on('pageerror', (e) => console.error('page error:', e.message));

await page.goto(`${url}?rects=${rects}`);
await page.waitForSelector('#bench', { timeout: 60000 });
await page.waitForTimeout(seconds * 1000);
await browser.close();

// Drop the first two samples (warm-up) and report the median of the rest.
const steady = samples.slice(2);
if (steady.length === 0) {
  console.error('no fps samples; got', samples);
  process.exit(1);
}
const fps = steady.map((s) => s.fps).sort((a, b) => a - b);
const median = fps[Math.floor(fps.length / 2)];
const frame = steady.map((s) => s.ms).sort((a, b) => a - b);
console.log(`rects=${steady[0].rects} samples=${steady.length} fps: median=${median} min=${fps[0]} max=${fps[fps.length - 1]}  ms/frame: median=${frame[Math.floor(frame.length / 2)]}`);
console.log('all:', samples.map((s) => s.fps).join(' '));
