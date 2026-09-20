// Browser smoke test for the sample app built with MicroHs (see scripts/playwright-mhs.sh).
// Loads the page served on port 8080, clicks the counter buttons, and prints
// SUCCESS or ERROR (the same protocol as playwright.ts).
import { chromium } from 'playwright';

(async () => {
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();
  page.on('console', (msg) => console.log('[console:' + msg.type() + '] ' + msg.text()));
  page.on('pageerror', (err) => console.log('[pageerror] ' + err.message));
  await page.goto('http://127.0.0.1:8080');
  try {
    await page.waitForSelector('button', { timeout: 20000 });
    const text = async () => (await page.locator('body').innerText()).replace(/\s+/g, '');
    const before = await text();
    await page.getByText('+', { exact: true }).click();
    await page.getByText('+', { exact: true }).click();
    await page.getByText('-', { exact: true }).click();
    await page.waitForTimeout(300);
    const after = await text();
    console.log(`before: ${before}, after: ${after}`);
    const ok = before.includes('+0-') && after.includes('+1-');
    console.log(ok ? 'SUCCESS' : 'ERROR');
    await browser.close();
    process.exit(ok ? 0 : 1);
  } catch (e) {
    console.log('ERROR ' + e.message);
    await browser.close();
    process.exit(1);
  }
})();
