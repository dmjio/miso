const { chromium } = require('playwright');

(async () => {
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

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
  await page.goto('http://127.0.0.1:8080');

})();
