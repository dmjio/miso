// The browser process's own output (its stderr), kept quietly and printed
// only when the page or browser goes away, to say why (a fatal check, a
// signal).  Playwright prints it under DEBUG=pw:browser, on our stderr; keep
// those lines here instead of printing them.
const browserLog = [];
process.env.DEBUG = [process.env.DEBUG, 'pw:browser'].filter(Boolean).join(',');
const stderrWrite = process.stderr.write.bind(process.stderr);
process.stderr.write = (chunk, ...rest) => {
  const text = String(chunk);
  if (!text.includes('pw:browser')) return stderrWrite(chunk, ...rest);
  // Skip the page's console messages, which the browser echoes (we print
  // those already): keep the browser's own.
  for (const line of text.split('\n')) if (line && !line.includes(':INFO:CONSOLE')) browserLog.push(line);
  while (browserLog.length > 50) browserLog.shift();
  return true;
};
const { chromium } = require('playwright');

// The tests report SUCCESS or ERROR on the console when they finish.  If the
// page dies or hangs instead, fail after this long rather than wait forever.
const TIMEOUT_MS = 3 * 60 * 1000;

(async () => {
  const browser = await chromium.launch({ headless: true });
  const page = await browser.newPage();

  // Seconds since start, to place events in a log.
  const start = Date.now();
  const at = () => `[${((Date.now() - start) / 1000).toFixed(1)}s]`;
  // Set once we close the browser ourselves, so that the close and
  // disconnect events that follow are not reported as failures.
  let finishing = false;
  const finish = (code) => {
    finishing = true;
    browser.close ();
    process.exit(code);
  };
  const fail = (why) => {
    console.log(why);
    finish(1);
  };
  const failWithBrowserLog = (why) => {
    console.log(why);
    console.log('last browser process output:');
    for (const line of browserLog) console.log(`  ${line}`);
    finish(1);
  };
  // On a hang, report where the run stopped before failing. Each probe is
  // capped: if the page cannot even answer, its main thread is busy (an
  // infinite loop) rather than idle (a deadlock).
  const probe = (label, fn) =>
    Promise.race([
      page.evaluate(fn).then((v) => `${label}: ${JSON.stringify(v)}`),
      new Promise((res) => setTimeout(() => res(`${label}: page did not answer within 5s (main thread busy)`), 5000)),
    ]).catch((e) => `${label}: ${e}`);
  setTimeout(async () => {
    console.log(await probe('test phase', () => globalThis.misoTestPhase));
    console.log(await probe('body', () => document.body.innerHTML.slice(0, 500)));
    // GHC JS backend runtime, if present: the running thread, and the
    // threads that are blocked and on what.
    console.log(await probe('js rts', () => {
      const g = globalThis;
      if (typeof g.h$currentThread === 'undefined') return 'no h$currentThread global (WASM backend, or JS RTS not global)';
      // See rts/js/thread.js (h$threads: h$Queue, h$blocked: h$Set) and
      // rts/js/structs.js (h$Queue.iter returns a function; h$Set.values an array).
      const desc = (t) => t && {
        thread: typeof g.h$threadString === 'function' ? g.h$threadString(t) : t.tid,
        status: t.status,
        blockedOn: t.blockedOn ? String((t.blockedOn.constructor && t.blockedOn.constructor.name) || t.blockedOn) : null,
      };
      const runnable = [];
      if (g.h$threads && g.h$threads.iter) {
        const next = g.h$threads.iter();
        for (let t = next(); t !== null && t !== undefined; t = next()) runnable.push(desc(t));
      }
      const blocked = g.h$blocked && g.h$blocked.values ? g.h$blocked.values().filter((t) => t).map(desc) : [];
      return { current: desc(g.h$currentThread), runnable, blocked };
    }));
    fail(`${at()} ERROR: no SUCCESS or ERROR after ${TIMEOUT_MS / 1000}s`);
  }, TIMEOUT_MS);

  // Capture console output
  page.on('console', (msg) => {
    if (msg.text() === "SUCCESS") {
        console.log(`${at()} SUCCESS`);
        finish(0);
    } else if (msg.text() === "ERROR") {
        console.log(`${at()} ERROR (reported by the tests)`);
        finish(1);
    } else {
      console.log(msg.text());
    }
  });
  // What the console does not show: uncaught exceptions (and WebAssembly
  // traps), and a crashed page.
  page.on('pageerror', (err) => console.log(`pageerror: ${err.stack || err}`));
  page.on('crash', () => failWithBrowserLog(`${at()} ERROR: the page crashed`));
  // A page or browser that goes away without crashing (closed, or the browser
  // process died): fail now, rather than at the timeout.
  page.on('close', () => { if (!finishing) failWithBrowserLog(`${at()} ERROR: the page closed`); });
  browser.on('disconnected', () => { if (!finishing) failWithBrowserLog(`${at()} ERROR: the browser disconnected (its process exited)`); });
  await page.goto('http://127.0.0.1:8080');

})();
