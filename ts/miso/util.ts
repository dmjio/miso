import { VTreeType, PRNG, VComp, VNode, VTree, Response } from './types';

/* current miso version */
export const version: string = '1.9.0.0';

/* various utilities */
export function callFocus(id: string, delay: number): void {
  var setFocus = function () {
    var e = document.getElementById(id);
    if (e && e.focus) e.focus();
  };
  delay > 0 ? setTimeout(setFocus, delay) : setFocus();
}

export function callBlur(id: string, delay: number): void {
  var setBlur = function () {
    var e = document.getElementById(id);
    if (e && e.blur) e.blur();
  };
  delay > 0 ? setTimeout(setBlur, delay) : setBlur();
}

export function callSelect(id: string, delay: number): void {
  var setSelect = function () {
    var e = document.getElementById(id);
    if (e && typeof e['select'] === 'function') (e as HTMLInputElement).select();
  };
  delay > 0 ? setTimeout(setSelect, delay) : setSelect();
}

export function callSetSelectionRange(id: string, start: number, end: number, delay: number): void {
  var setSetSelectionRange = function () {
    var e = document.getElementById(id);
    if (e && typeof e['setSelectionRange'] === 'function') (e as HTMLInputElement).setSelectionRange(start, end, 'none');
  };
  delay > 0 ? setTimeout(setSetSelectionRange, delay) : setSetSelectionRange();
}

export function fetchCore (
  url : string,
  method : string,
  body : any,
  requestHeaders : Record<string,string>,
  successful: (response: Response) => void,
  errorful: (response: Response) => void,
  responseType: string /* dmj: expected response type */
): any
{
  var options = { method, headers: requestHeaders };
  if (body) {
    options['body'] = body;
  }
  let headers = {};
  let status = null;
  try {
    fetch (url, options)
        .then(response => {
          status = response.status;
          for (const [key, value] of response.headers) {
             headers[key] = value;
          }
          if (!response.ok) {
            throw new Error(response.statusText);
          }
          if (responseType == 'json') {
            return response.json();
          } else if (responseType == 'text') {
            return response.text();
          } else if (responseType === 'arrayBuffer') {
            return response.arrayBuffer();
          } else if (responseType === 'blob') {
            return response.blob();
          } else if (responseType === 'bytes') {
            return response.bytes();
          } else if (responseType === 'formData') {
            return response.formData();
          } else if (responseType === 'none') {
            return successful({error:null, body: null, headers, status});
          }
        })
        .then((body) => successful({error: null, body, headers, status}))
        .catch((body) => errorful({error: null, body, headers, status})); /* error callback */
  } catch (err) {
      errorful({ body: null, error: err.message, headers, status});
  }
}

export function websocketConnect (
    url: string,
    onOpen,
    onClose,
    onMessageText,
    onMessageJSON,
    onMessageBLOB,
    onMessageArrayBuffer,
    onError,
    textOnly : boolean,
): WebSocket {
  try {
    let socket = new WebSocket(url);
    socket.onopen = function () {
      onOpen();
    };
    socket.onclose = function (e) {
      onClose(e);
    };
    socket.onerror = function (error) {
      console.error (error);
      onError("WebSocket error received");
    };
    socket.onmessage = function (msg) {
      if (typeof msg.data === "string") {
        try {
            if (textOnly) {
              if (onMessageText) onMessageText(msg.data)
              return;
            }
            const json = JSON.parse (msg.data);
            if (onMessageJSON) onMessageJSON(json);
        } catch (err) {
            if (textOnly && onMessageText) {
              onMessageText(msg.data)
            } else {
              onError(err.message)
            }
        }
      } else if (msg.data instanceof Blob) {
          if (onMessageBLOB) onMessageBLOB (msg.data)
      } else if (msg.data instanceof ArrayBuffer) {
          if (onMessageArrayBuffer) onMessageArrayBuffer (msg.data)
      } else {
        console.error ("Received unknown message type from WebSocket", msg);
        onError ("Unknown message received from WebSocket");
      }
    };
    return socket;
  } catch (err) {
    onError (err.message);
  }
}

export function websocketClose (
  socket
): void {
  if (socket) {
    socket.close();
    socket = null;
  }
}

export function websocketSend (
  socket, message
): void {
  if (message && socket && socket.readyState === WebSocket.OPEN) {
      socket.send(message);
  }
}

export function eventSourceConnect (
    url: string,
    onOpen,
    onMessageText,
    onMessageJSON,
    onError,
    textOnly: boolean,
): EventSource {
  try {
    let eventSource = new EventSource(url);
    eventSource.onopen = function () {
        onOpen();
    };
    eventSource.onerror = function () {
        onError('EventSource error received');
    };
    eventSource.onmessage = function (msg) {
       try {
            if (textOnly) {
              if (onMessageText) onMessageText(msg.data)
              return;
            }
            const json = JSON.parse (msg.data);
            if (onMessageJSON) onMessageJSON(json);
        } catch (err) {
            if (textOnly && onMessageText) {
              onMessageText(msg.data)
            } else {
              onError(err.message)
            }
        }
    };
    return eventSource;
  } catch (err) {
      onError (err.message);
  }
}

export function eventSourceClose (
  eventSource: EventSource
): void {
  if (eventSource) {
    eventSource.close();
    eventSource = null;
  }
}

/* Initializes the classList field to an Empty Set
   Populates with the contents of 'classes'
 */
export function populateClass<T> (
  vnode: VNode<T>,
  classes: Array<string>
): void {
    if (!(vnode.classList)) {
      vnode.classList = new Set();
    }
    for (const str of classes) {
        for (const c of str.trim().split(' ')) {
           if (c) vnode.classList.add(c);
        }
    }
}

export function updateRef <T> (current: VTree<T> , latest: VTree<T>) : void {
  if (!(current.parent)) {
     return; // at root, do nothing
  }
  latest.nextSibling = current.nextSibling ? null : current.nextSibling;
  latest.parent = current.parent;
  // invariant, parent is always VComp<T>, safe cast
  (current.parent as VComp<T>).child = latest;
}

export function inline(code, context = {}) {
  const keys = Object.keys(context);
  const values = Object.values(context);
  const func = new Function(...keys, code);
  return func(...values);
}

/*
--- Determine typeOf for FromJSVal Value instance
--- 0. null
--- 1. number
--- 2. string
--- 3. bool
--- 4. array
--- 5. object
*/
export function typeOf (x) : number {
  if (x === null || x === undefined) return 0;
  if (typeof(x) === 'number') return 1;
  if (typeof(x) === 'string') return 2;
  if (typeof(x) === 'boolean') return 3;
  if (Array.isArray(x)) return 4;
  return 5;
}

/* Add splitmix32 random seed functionality */
export function splitmix32(a : number) : PRNG {
  return function() {
    a |= 0; a = a + 0x9e3779b9 | 0;
    var t = a ^ a >>> 15;
    t = Math.imul(t, 0x85ebca6b);
    t = t ^ t >>> 13;
    t = Math.imul(t, 0xc2b2ae35);
    return ((t ^ t >>> 16) >>> 0) / 4294967296;
  }
}

/* crypto.getRandomValues() */
export function getRandomValues () : number {
  const array = new Uint32Array(1);
  return crypto.getRandomValues(array)[0];
}

/* Math.random() */
export function mathRandom() : number {
  return Math.random();
}

// Extract DOM reference from any VTree (handles VComp drilling)
export function getDOMRef<T>(tree: VTree<T>): T {
  switch (tree.type) {
    case VTreeType.VComp:
      return drill(tree);
    default:
      return tree.domRef;
  }
}

//c.child should never be null
export function drill<T>(c: VComp<T>): T {
  if (!c.child) throw new Error ("'drill' called on an unmounted Component. This should never happen, please make an issue.");
  switch (c.child.type) {
    case VTreeType.VComp:
      return drill (c.child)
    default:
      return c.child.domRef;
  }
}

export function bts () : boolean {
  return __BACKGROUND__;
}
