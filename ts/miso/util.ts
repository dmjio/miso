import { VNode } from './types';

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

export function setBodyComponent(componentId: string): void {
  document.body.setAttribute('data-component-id', componentId);
}

export function fetchFFI (
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
        return response.text();
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


/* current miso version */
export const version: string = '1.9.0.0';
