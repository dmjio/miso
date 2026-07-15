// echo-server.ts
// A local CORS-enabled HTTP server on port 8081 that mimics the httpbin.org
// endpoints used by Miso.Fetch integration tests.  Run alongside the static
// http-server so that browser-side fetch calls can reach a real network
// endpoint without being blocked by CORS.

const PORT = 8081;

const CORS_HEADERS: Record<string, string> = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'GET, POST, PUT, OPTIONS',
  'Access-Control-Allow-Headers': 'Content-Type, Accept, Authorization',
  'Access-Control-Max-Age': '86400',
};

// Exact bytes returned by httpbin.org /bytes/16?seed=42 — the seed parameter
// makes the endpoint deterministic, so the tests can assert on exact content.
const SEEDED_BYTES = new Uint8Array([
  0x39, 0x0c, 0x8c, 0x7d, 0x72, 0x47, 0x34, 0x2c,
  0xd8, 0x10, 0x0f, 0x2f, 0x6f, 0x77, 0x0d, 0x65,
]);

// Minimal 1×1 transparent RGBA PNG — first 8 bytes are the standard PNG
// file signature that the tests assert on.
const PNG_BYTES = new Uint8Array([
  0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a,  // PNG signature
  0x00, 0x00, 0x00, 0x0d, 0x49, 0x48, 0x44, 0x52,  // IHDR chunk
  0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01,  // 1×1 pixels
  0x08, 0x06, 0x00, 0x00, 0x00, 0x1f, 0x15, 0xc4,  // 8-bit RGBA
  0x89, 0x00, 0x00, 0x00, 0x0b, 0x49, 0x44, 0x41,  // CRC, IDAT chunk
  0x54, 0x78, 0xda, 0x63, 0x60, 0x00, 0x02, 0x00,  // zlib-compressed pixel
  0x00, 0x05, 0x00, 0x01, 0xe9, 0xfa, 0xdc, 0xd8,  // CRC
  0x00, 0x00, 0x00, 0x00, 0x49, 0x45, 0x4e, 0x44,  // IEND chunk
  0xae, 0x42, 0x60, 0x82,                           // CRC
]);

function corsHeaders(extra: Record<string, string> = {}): Record<string, string> {
  return { ...CORS_HEADERS, ...extra };
}

function jsonResponse(data: unknown, status = 200): Response {
  return new Response(JSON.stringify(data), {
    status,
    headers: corsHeaders({ 'Content-Type': 'application/json' }),
  });
}

const server = Bun.serve({
  port: PORT,
  async fetch(req: Request): Promise<Response> {
    const url = new URL(req.url);
    const path = url.pathname;
    const method = req.method;

    // CORS preflight
    if (method === 'OPTIONS') {
      return new Response(null, { status: 204, headers: CORS_HEADERS });
    }

    // GET /get → {"url": <request URL>}
    if (path === '/get' && method === 'GET') {
      return jsonResponse({ url: req.url });
    }

    // GET /status/<code> → empty body with that HTTP status
    if (path.startsWith('/status/') && method === 'GET') {
      const code = parseInt(path.slice('/status/'.length), 10);
      if (!isNaN(code)) {
        return new Response('', { status: code, headers: CORS_HEADERS });
      }
    }

    // POST /post or PUT /put
    if ((path === '/post' || path === '/put') && (method === 'POST' || method === 'PUT')) {
      const ct = req.headers.get('content-type') ?? '';
      if (ct.includes('application/json')) {
        // Echo the JSON body back under the "json" key, matching httpbin.org's format
        const body = await req.json();
        return jsonResponse({ json: body });
      }
      // For all other content types (blob, arraybuffer, formdata, image, text …)
      // just confirm receipt with 200 OK
      return jsonResponse({ status: 'ok' });
    }

    // GET /robots.txt → plain text containing "Disallow"
    if (path === '/robots.txt' && method === 'GET') {
      return new Response('User-agent: *\nDisallow: /private/', {
        status: 200,
        headers: corsHeaders({ 'Content-Type': 'text/plain' }),
      });
    }

    // GET /image/png → a minimal valid PNG file
    if (path === '/image/png' && method === 'GET') {
      return new Response(PNG_BYTES, {
        status: 200,
        headers: corsHeaders({ 'Content-Type': 'image/png' }),
      });
    }

    // GET /bytes/<n> → the pre-computed seeded byte sequence
    if (path.startsWith('/bytes/') && method === 'GET') {
      return new Response(SEEDED_BYTES, {
        status: 200,
        headers: corsHeaders({ 'Content-Type': 'application/octet-stream' }),
      });
    }

    return new Response('Not Found', { status: 404, headers: CORS_HEADERS });
  },
});

console.log(`echo-server listening on http://127.0.0.1:${PORT}`);
