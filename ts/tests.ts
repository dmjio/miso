const { JSDOM } = require('jsdom');

// Create a basic DOM environment
globalThis.initJSDOM = function () {
  const dom = new JSDOM('<!DOCTYPE html><html><body></body></html>', {
    url: 'http://localhost',
    referrer: 'https://example.com/',
    contentType: 'text/html',
    includeNodeLocations: true,
    storageQuota: 10000000
  });

  // Make DOM available globally
  global.window = dom.window;
  global.document = dom.window.document;
  global.navigator = dom.window.navigator;
  global.HTMLElement = dom.window.HTMLElement;
  global.HTMLDivElement = dom.window.HTMLDivElement;
  // Add other globals as needed
}
