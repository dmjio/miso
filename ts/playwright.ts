const { chromium } = require('playwright');

// The tests report SUCCESS or ERROR on the console when they finish.  If the
// page dies or hangs instead, fail after this long rather than wait forever.
const TIMEOUT_MS = 3 * 60 * 1000;

(async () => {
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  const fail = (why) => {
    console.log(why);
    browser.close ();
    process.exit(1);
  };
  setTimeout(() => fail(`ERROR: no SUCCESS or ERROR after ${TIMEOUT_MS / 1000}s`), TIMEOUT_MS);

  // Capture console output
  page.on('console', (msg) => {
    if (msg.text() === "SUCCESS") {
        browser.close ();
        process.exit(0);
    } else if (msg.text() === "ERROR") {
        browser.close ();
        process.exit(1);
    } else {
      console.log(msg.text());
    }
  });
  // What the console does not show: uncaught exceptions (and WebAssembly
  // traps), and a crashed page.
  page.on('pageerror', (err) => console.log(`pageerror: ${err.stack || err}`));
  page.on('crash', () => fail('ERROR: the page crashed'));
  await page.goto('http://127.0.0.1:8080');

})();
