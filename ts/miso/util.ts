import { VNode } from './types';

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

export function fetchJSON (
  url : string,
  method : string,
  body : any,
  headers : Record<string,string>,
  successful: (string) => void,
  errorful: (string) => void
): void
{
  var options = { method, headers };
  if (body) {
    options['body'] = body;
  }
  fetch (url, options)
      .then(response => {
        if (!response.ok) {
          throw new Error(response.statusText);
        }
        return response.json();
      })
    .then(successful) /* success callback */
    .catch(errorful); /* error callback */
}

/*
   'shouldSync'
   dmj: Used to determine if we should enter `syncChildren`

*/
export function shouldSync (
  node: VNode
): boolean {
    /* cannot sync on null children */
    if (node.children.length === 0) {
        return false;
    }
    /* only sync if keys exist on all children  */
    var enterSync = true;
    for (const child of node.children) {
        if (!child.key) {
          enterSync = false;
          break;
        }
    }
    return enterSync;
}

/*
   'getParentComponentId'
   dmj: Used to fetch the parent's componentId

   Climbs up the tree, finds the immediate component ancestor (parent) and returns its componentId
   This should be called on the DOMRef of a VComp, otherwise it will return the current componentId.

*/
export function getParentComponentId (
  vcompNode: ParentNode
): number {
    var climb = function (node : ParentNode) {
      let parentComponentId = null;
      while (node && node.parentNode) {
          if ('componentId' in node.parentNode) {
              parentComponentId = node.parentNode['componentId'];
              break;
          }
          node = node.parentNode;
      }
      return parentComponentId;
    }
    return climb (vcompNode);
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
