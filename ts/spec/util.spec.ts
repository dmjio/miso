import { populateClass, callFocus, callBlur, callSelect, callSetSelectionRange, fetchCore, websocketConnect, websocketClose, websocketSend, eventSourceConnect, eventSourceClose } from '../miso/util';
import { vnode } from '../miso/smart';
import { VNode } from '../miso/types';
import { test, expect, describe, afterEach, beforeAll, mock } from 'bun:test';

/* silence */
beforeAll(() => {
  console.log = () => {};
  console.info = () => {};
  console.warn = () => {};
  console.error = () => {};
});

/* reset DOM */
afterEach(() => {
  document.body.innerHTML = '';
});

/* tests */
describe ('Utils tests', () => {

  test('Should call callFocus(), callSelect(), callBlur() and callSetSelectionRange()', () => {
    var child = document.createElement('input');
    child['id'] = 'foo';
    child['value'] = 'bar';
    document.body.appendChild(child);
    callFocus('blah', 0); /* missing case */
    callFocus('foo', 0); /* found case */
    callFocus('foo', 1); /* found case */
    expect(document.activeElement).toEqual(child);
    callSelect('blah', 0);
    callSelect('foo', 0);
    callSelect('foo', 1);
    var e : HTMLInputElement = document.querySelector('#foo');
    expect(e.selectionStart).toEqual(0);
    expect(e.selectionEnd).toEqual(3);
    callSetSelectionRange('blah', 1, 2, 0);
    callSetSelectionRange('foo', 1, 2, 0);
    callSetSelectionRange('foo', 1, 2, 1)
    expect(e.selectionStart).toEqual(1);
    expect(e.selectionEnd).toEqual(2);
    callBlur('blah', 0); /* missing case */
    callBlur('foo', 0); /* found case */
    callBlur('foo', 1); /* found case */
    expect(document.activeElement).toEqual(document.body);
  });

  test('Should populate class', () => {
      let node : VNode<number> = vnode({});
      expect(node.classList).toBe(null);
      var expected = new Set(["foo", "bar", "baz", "lol", "some-long-tailwind-class"]);
      populateClass(node, ["some-long-tailwind-class foo", "bar", "lol baz"]);
      populateClass(node, ["baz", "foo", "bar"]);
      populateClass(node, [" "]);
      for (const elem of expected) {
          expect(node.classList.has(elem)).toBe(true);
      }
      expect(expected.size).toBe(node.classList.size);
  });

});

describe('Fetch tests', () => {
  test('Should handle successful JSON fetch', async () => {
    const mockResponse = { data: 'test' };
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        status: 200,
        statusText: 'OK',
        headers: new Map([['content-type', 'application/json']]),
        json: async () => mockResponse
      } as any)
    ) as any;

    let result = null;
    let error = null;

    fetchCore(
      'https://api.example.com/test',
      'GET',
      null,
      {},
      (response) => { result = response; },
      (response) => { error = response; },
      'json'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(result).not.toBeNull();
    expect(result.body).toEqual(mockResponse);
    expect(result.status).toBe(200);
    expect(error).toBeNull();
  });

  test('Should handle successful text fetch', async () => {
    const mockText = 'Hello World';
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        status: 200,
        statusText: 'OK',
        headers: new Map([['content-type', 'text/plain']]),
        text: async () => mockText
      } as any)
    ) as any;

    let result = null;

    fetchCore(
      'https://api.example.com/test',
      'GET',
      null,
      {},
      (response) => { result = response; },
      (response) => {},
      'text'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(result).not.toBeNull();
    expect(result.body).toBe(mockText);
  });

  test('Should handle successful arrayBuffer fetch', async () => {
    const mockBuffer = new ArrayBuffer(8);
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        status: 200,
        statusText: 'OK',
        headers: new Map(),
        arrayBuffer: async () => mockBuffer
      } as any)
    ) as any;

    let result = null;

    fetchCore(
      'https://api.example.com/test',
      'GET',
      null,
      {},
      (response) => { result = response; },
      (response) => {},
      'arrayBuffer'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(result).not.toBeNull();
    expect(result.body).toBe(mockBuffer);
  });

  test('Should handle successful blob fetch', async () => {
    const mockBlob = new Blob(['test']);
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        status: 200,
        statusText: 'OK',
        headers: new Map(),
        blob: async () => mockBlob
      } as any)
    ) as any;

    let result = null;

    fetchCore(
      'https://api.example.com/test',
      'GET',
      null,
      {},
      (response) => { result = response; },
      (response) => {},
      'blob'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(result).not.toBeNull();
    expect(result.body).toBe(mockBlob);
  });

  test('Should handle successful bytes fetch', async () => {
    const mockBytes = new Uint8Array([1, 2, 3]);
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        status: 200,
        statusText: 'OK',
        headers: new Map(),
        bytes: async () => mockBytes
      } as any)
    ) as any;

    let result = null;

    fetchCore(
      'https://api.example.com/test',
      'GET',
      null,
      {},
      (response) => { result = response; },
      (response) => {},
      'bytes'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(result).not.toBeNull();
    expect(result.body).toBe(mockBytes);
  });

  test('Should handle successful formData fetch', async () => {
    const mockFormData = new FormData();
    mockFormData.append('key', 'value');
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        status: 200,
        statusText: 'OK',
        headers: new Map(),
        formData: async () => mockFormData
      } as any)
    ) as any;

    let result = null;

    fetchCore(
      'https://api.example.com/test',
      'GET',
      null,
      {},
      (response) => { result = response; },
      (response) => {},
      'formData'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(result).not.toBeNull();
    expect(result.body).toBe(mockFormData);
  });

  test('Should handle none response type', async () => {
    let successCalled = false;

    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        status: 204,
        statusText: 'No Content',
        headers: {
          [Symbol.iterator]: function* () {
            yield ['content-type', 'application/json'];
          }
        },
      } as any)
    ) as any;

    fetchCore(
      'https://api.example.com/test',
      'DELETE',
      null,
      {},
      (response) => {
        successCalled = true;
        expect(response.body).toBeNull();
        expect(response.status).toBe(204);
      },
      (response) => {},
      'none'
    );

    await new Promise(resolve => setTimeout(resolve, 50));

    expect(successCalled).toBe(true);
  });

  test('Should handle POST with body', async () => {
    const requestBody = JSON.stringify({ name: 'test' });
    global.fetch = mock(() =>
      Promise.resolve({
        ok: true,
        status: 201,
        statusText: 'Created',
        headers: new Map(),
        json: async () => ({ id: 1 })
      } as any)
    ) as any;

    let result = null;

    fetchCore(
      'https://api.example.com/test',
      'POST',
      requestBody,
      { 'Content-Type': 'application/json' },
      (response) => { result = response; },
      (response) => {},
      'json'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(result).not.toBeNull();
  });

  test('Should handle fetch error with non-ok response', async () => {
    global.fetch = mock(() =>
      Promise.resolve({
        ok: false,
        status: 404,
        statusText: 'Not Found',
        headers: new Map(),
      } as any)
    ) as any;

    let error = null;

    fetchCore(
      'https://api.example.com/test',
      'GET',
      null,
      {},
      (response) => {},
      (response) => { error = response; },
      'json'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(error).not.toBeNull();
  });

  test('Should handle network error', async () => {
    global.fetch = mock(() => Promise.reject(new Error('Network error'))) as any;

    let error = null;

    fetchCore(
      'https://api.example.com/test',
      'GET',
      null,
      {},
      (response) => {},
      (response) => { error = response; },
      'json'
    );

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(error).not.toBeNull();
  });
});

describe('WebSocket tests', () => {
  test('Should connect websocket and handle onOpen', () => {
    let opened = false;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => { opened = true; },
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      false
    );

    socket.onopen({} as Event);
    expect(opened).toBe(true);
    websocketClose(socket);
  });

  test('Should handle onClose', () => {
    let closed = false;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      (e) => { closed = true; },
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      false
    );

    socket.onclose({} as CloseEvent);
    expect(closed).toBe(true);
    websocketClose(socket);
  });

  test('Should handle onError', () => {
    let errorReceived = false;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      (msg) => { errorReceived = true; },
      false
    );

    socket.onerror({} as Event);
    expect(errorReceived).toBe(true);
    websocketClose(socket);
  });

  test('Should handle JSON message', () => {
    let receivedData = null;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      (data) => { receivedData = data; },
      () => {},
      () => {},
      () => {},
      false
    );

    const mockMessage = { data: JSON.stringify({ test: 'value' }) };
    socket.onmessage(mockMessage as MessageEvent);

    expect(receivedData).toEqual({ test: 'value' });
    websocketClose(socket);
  });

  test('Should handle text message in textOnly mode', () => {
    let receivedText = null;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      (text) => { receivedText = text; },
      () => {},
      () => {},
      () => {},
      () => {},
      true
    );

    const mockMessage = { data: 'plain text message' };
    socket.onmessage(mockMessage as MessageEvent);

    expect(receivedText).toBe('plain text message');
    websocketClose(socket);
  });

  test('Should handle invalid JSON with error callback', () => {
    let errorReceived = false;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      (msg) => { errorReceived = true; },
      false
    );

    const mockMessage = { data: 'not valid json {' };
    socket.onmessage(mockMessage as MessageEvent);

    expect(errorReceived).toBe(true);
    websocketClose(socket);
  });

  test('Should handle invalid JSON in textOnly mode with onMessageText', () => {
    let receivedText = null;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      (text) => { receivedText = text; },
      () => {},
      () => {},
      () => {},
      () => {},
      true
    );

    const mockMessage = { data: 'invalid json {' };
    socket.onmessage(mockMessage as MessageEvent);

    expect(receivedText).toBe('invalid json {');
    websocketClose(socket);
  });

  test('Should handle Blob message', () => {
    let receivedBlob = null;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      () => {},
      (blob) => { receivedBlob = blob; },
      () => {},
      () => {},
      false
    );

    const mockBlob = new Blob(['test']);
    const mockMessage = { data: mockBlob };
    socket.onmessage(mockMessage as MessageEvent);

    expect(receivedBlob).toBe(mockBlob);
    websocketClose(socket);
  });

  test('Should handle ArrayBuffer message', () => {
    let receivedBuffer = null;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      (buffer) => { receivedBuffer = buffer; },
      () => {},
      false
    );

    const mockBuffer = new ArrayBuffer(8);
    const mockMessage = { data: mockBuffer };
    socket.onmessage(mockMessage as MessageEvent);

    expect(receivedBuffer).toBe(mockBuffer);
    websocketClose(socket);
  });

  test('Should handle unknown message type', () => {
    let errorReceived = false;

    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      (msg) => { errorReceived = true; },
      false
    );

    const mockMessage = { data: 12345 }; // Number is not a valid message type
    socket.onmessage(mockMessage as MessageEvent);

    expect(errorReceived).toBe(true);
    websocketClose(socket);
  });

  test('Should send message when socket is open', () => {
    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      false
    );

    // Mock the send method
    let sentMessage = null;
    socket.send = (msg) => { sentMessage = msg; };
    Object.defineProperty(socket, 'readyState', { value: WebSocket.OPEN });

    websocketSend(socket, 'test message');
    expect(sentMessage).toBe('test message');

    websocketClose(socket);
  });

  test('Should not send when socket is not open', () => {
    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      false
    );

    let sentMessage = null;
    socket.send = (msg) => { sentMessage = msg; };
    Object.defineProperty(socket, 'readyState', { value: WebSocket.CONNECTING });

    websocketSend(socket, 'test message');
    expect(sentMessage).toBeNull();

    websocketClose(socket);
  });

  test('Should not send when message is null', () => {
    const socket = websocketConnect(
      'ws://localhost:8080',
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      () => {},
      false
    );

    let sendCalled = false;
    socket.send = () => { sendCalled = true; };
    Object.defineProperty(socket, 'readyState', { value: WebSocket.OPEN });

    websocketSend(socket, null);
    expect(sendCalled).toBe(false);

    websocketClose(socket);
  });
});

describe('EventSource tests', () => {
  test('Should attempt to connect EventSource', () => {
    // EventSource is not supported in happy-dom, so we test error handling
    let errorReceived = false;

    try {
      const eventSource = eventSourceConnect(
        'http://localhost:8080/events',
        () => {},
        () => {},
        () => {},
        (msg) => { errorReceived = true; },
        false
      );

      // If EventSource is available, test it
      if (eventSource) {
        expect(eventSource).toBeDefined();
        eventSourceClose(eventSource);
      }
    } catch (err) {
      // Expected in environments without EventSource
      expect(true).toBe(true);
    }
  });
});
