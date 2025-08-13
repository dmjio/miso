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
    onError,
    onMessage,
): WebSocket {
  let socket = new WebSocket(url);
  socket.onopen = function () {
    onOpen();
  };
  socket.onclose = function (e) {
    onClose(e);
  };
  socket.onerror = function (error) {
    onError(error);
  };
  socket.onmessage = function (msg) {
    onMessage(msg.data);
  };
  return socket;
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
