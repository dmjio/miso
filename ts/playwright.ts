const http = require('http');
const url = require('url');
const { chromium } = require('playwright');

let browser;
let server;
let isShuttingDown = false;
const TEST_TIMEOUT = 10000;    // 10 seconds for background tests
const WAIT_TIMEOUT = 15000;    // 15 seconds for synchronous tests

// ======================
// BROWSER MANAGEMENT
// ======================
async function initializeBrowser() {
  browser = await chromium.launch({ headless: true });
  console.log('✅ Browser launched successfully');
}

async function closeBrowser() {
  if (!browser) return;
  try {
    await browser.close();
    console.log('✅ Browser closed successfully');
  } catch (e) {
    console.error('❌ Error closing browser:', e.message);
  }
}

// ======================
// TEST EXECUTION CORE
// ======================
async function executeTestCore(page, port, timeout) {
  return new Promise(async (resolve) => {
    let testCompleted = false;
    
    // Console message handler
    const consoleHandler = (msg) => {
      if (testCompleted) return;
      const text = msg.text();
      
      if (text === 'SUCCESS' || text === 'ERROR') {
        testCompleted = true;
        clearTimeout(timeoutId);
        page.removeListener('console', consoleHandler);
        resolve({ status: text });
      }
    };

    page.on('console', consoleHandler);

    // Timeout handler
    const timeoutId = setTimeout(() => {
      if (testCompleted) return;
      testCompleted = true;
      page.removeListener('console', consoleHandler);
      resolve({ status: 'TIMEOUT' });
    }, timeout);

    // Navigation
    try {
      await page.goto(`http://127.0.0.1:${port}`, {
        timeout: timeout - 1000,
        waitUntil: 'networkidle'
      });
    } catch (navError) {
      if (!testCompleted) {
        testCompleted = true;
        clearTimeout(timeoutId);
        page.removeListener('console', consoleHandler);
        resolve({ 
          status: 'NAVIGATION_ERROR', 
          error: navError.message 
        });
      }
    }
  });
}

async function runTestInBackground(port) {
  if (isShuttingDown) {
    console.log(`[PORT ${port}] ❌ Skipped - Server shutting down`);
    return;
  }

  let page;
  try {
    page = await browser.newPage();
    console.log(`[PORT ${port}] 🧪 Background test started`);
    
    const result = await executeTestCore(page, port, TEST_TIMEOUT);
    
    console.log(`[PORT ${port}] ✅ Background test result: ${result.status}`);
    await safePageClose(page, port, 'background_test');
  } catch (e) {
    console.error(`[PORT ${port}] ❌ Background test failed:`, e.message);
    if (page) await safePageClose(page, port, 'background_error');
  }
}

async function runTestAndWait(port) {
  let page;
  try {
    page = await browser.newPage();
    console.log(`[PORT ${port}] ⏳ Synchronous test started`);
    
    const result = await executeTestCore(page, port, WAIT_TIMEOUT);
    
    await safePageClose(page, port, 'synchronous_test');
    console.log(`[PORT ${port}] ✅ Synchronous test completed with: ${result.status}`);
    return result;
  } catch (e) {
    console.error(`[PORT ${port}] ❌ Synchronous test failed:`, e.message);
    if (page) await safePageClose(page, port, 'sync_error');
    throw e;
  }
}

async function safePageClose(page, port, reason) {
  if (!page) return;
  try {
    await page.close();
    console.log(`[PORT ${port}] 📄 Page closed (${reason})`);
  } catch (e) {
    console.error(`[PORT ${port}] ❌ Error closing page (${reason}):`, e.message);
  }
}

// ======================
// ENDPOINT HANDLERS
// ======================
async function handleTestRequest(req, res, port, waitForResult = false) {
  if (waitForResult) {
    try {
      const result = await runTestAndWait(port);
      
      switch (result.status) {
        case 'SUCCESS':
          res.writeHead(200, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ 
            status: 'success', 
            port,
            message: 'Test passed successfully'
          }));
          break;
        case 'ERROR':
          res.writeHead(500, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ 
            status: 'error', 
            port,
            message: 'Test failed with ERROR console message'
          }));
          break;
        case 'TIMEOUT':
          res.writeHead(504, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ 
            status: 'timeout', 
            port,
            message: `Test timed out after ${WAIT_TIMEOUT/1000} seconds`
          }));
          break;
        case 'NAVIGATION_ERROR':
          res.writeHead(502, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ 
            status: 'navigation_error', 
            port,
            message: result.error,
            details: 'Failed to load test page'
          }));
          break;
        default:
          res.writeHead(500, { 'Content-Type': 'application/json' });
          res.end(JSON.stringify({ 
            status: 'unknown', 
            port,
            message: 'Unknown test result'
          }));
      }
    } catch (e) {
      console.error(`[PORT ${port}] Synchronous test crashed:`, e.message);
      res.writeHead(500, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({ 
        status: 'server_error',
        port,
        message: 'Test execution crashed',
        error: e.message
      }));
    }
  } else {
    // Background execution
    console.log(`[PORT ${port}] 🚀 Test triggered (background)`);
    setImmediate(() => runTestInBackground(port).catch(e => 
      console.error(`[PORT ${port}] Background test error:`, e.message)
    ));

    res.writeHead(202, { 'Content-Type': 'application/json' });
    res.end(JSON.stringify({
      status: 'accepted',
      message: `Test started for port ${port} (running in background)`,
      port
    }));
  }
}

async function handleShutdownRequest(req, res) {
  console.log('🛑 Shutdown requested');
  
  res.writeHead(200, { 'Content-Type': 'application/json' });
  res.end(JSON.stringify({
    status: 'shutting_down',
    message: 'Server shutdown initiated'
  }));
  
  setImmediate(initiateShutdown);
}

// ======================
// SERVER MANAGEMENT
// ======================
async function initiateShutdown() {
  if (isShuttingDown) return;
  isShuttingDown = true;
  
  console.log('⏳ Initiating graceful shutdown...');
  
  server.close(() => {
    console.log('🔌 HTTP server closed');
  });

  try {
    await closeBrowser();
    console.log('👋 Server shutdown complete');
    process.exit(0);
  } catch (e) {
    console.error('💥 Critical shutdown error:', e.message);
    process.exit(1);
  }
}

function validatePort(portParam) {
  if (!portParam || isNaN(portParam)) {
    return { valid: false, error: 'Missing or invalid port parameter' };
  }

  const port = parseInt(portParam, 10);
  if (port < 1 || port > 65535) {
    return { valid: false, error: 'Port must be between 1 and 65535' };
  }
  
  return { valid: true, port };
}

function parseBooleanParam(param) {
  if (!param) return false;
  return ['true', '1', 'yes', 'on'].includes(param.toLowerCase());
}

// ======================
// SERVER SETUP
// ======================
async function startServer() {
  try {
    await initializeBrowser();
    
    server = http.createServer(async (req, res) => {
      const parsedUrl = url.parse(req.url, true);
      const { pathname, query } = parsedUrl;

      // Global shutdown guard
      if (isShuttingDown && pathname !== '/shutdown') {
        res.writeHead(503, { 'Content-Type': 'application/json' });
        return res.end(JSON.stringify({
          error: 'Service Unavailable',
          message: 'Server is shutting down'
        }));
      }

      // Route requests
      if (req.method === 'GET' && pathname === '/test') {
        const portValidation = validatePort(query.port);
        if (!portValidation.valid) {
          res.writeHead(400, { 'Content-Type': 'application/json' });
          return res.end(JSON.stringify({ error: portValidation.error }));
        }

        const waitForResult = parseBooleanParam(query.wait);
        return handleTestRequest(req, res, portValidation.port, waitForResult);
      }

      if (req.method === 'POST' && pathname === '/shutdown') {
        return handleShutdownRequest(req, res);
      }

      // Default 404
      res.writeHead(404, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({
        error: 'Not Found',
        endpoints: {
          test: 'GET /test?port=<PORT_NUMBER>&wait=<true|false>',
          shutdown: 'POST /shutdown'
        }
      }));
    });

    const PORT = 8060;
    server.listen(PORT, () => {
      console.log(`\n🚀 Server running at http://localhost:${PORT}`);
      console.log('_AVAILABLE ENDPOINTS_');
      console.log(`🔹 GET  /test?port=<PORT>&wait=<true|false>`);
      console.log(`   - wait=true:  Wait for test result (synchronous)`);
      console.log(`   - wait=false: Start test in background (asynchronous)`);
      console.log(`🔹 POST /shutdown        - Gracefully shut down server\n`);
    });

    // Handle process signals
    process.on('SIGINT', initiateShutdown);
    process.on('SIGTERM', initiateShutdown);
  } catch (e) {
    console.error('💥 Fatal startup error:', e.message);
    process.exit(1);
  }
}

// Start everything
startServer().catch(e => {
  console.error('💥 Server startup failed:', e.message);
  process.exit(1);
});
