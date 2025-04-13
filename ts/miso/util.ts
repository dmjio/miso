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

export function fetchJSON (
  url : string,
  method : string,
  body : any,
  successful: (string) => void,
  errorful: (string) => void
): void
{
  var options = {
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json',
    },
    method,
  };
  if (body) {
    options['body'] = body;
  }
  fetch (url, options)
      .then(response => {
        if (!response.ok) {
          throw new Error(response.statusMessage);
        }
        return response.json();
      })
    .then(successful) /* success callback */
    .catch(errorful); /* error callback */
}

export const version: string = '1.9.0.0';
