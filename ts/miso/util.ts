/* various utilities */
export function callFocus(id: string, delay: number) : void {
  var setFocus = function () {
    var e = document.getElementById(id);
    if (e && e.focus) e.focus();
  };
  delay > 0 ? setTimeout(setFocus, delay) : setFocus();
}

export function callBlur(id: string, delay: number) : void {
  var setBlur = function () {
    var e = document.getElementById(id);
    if (e && e.blur) e.blur();
  };
  delay > 0 ? setTimeout(setBlur, delay) : setBlur();
}

export function setBodyComponent(componentId: string) : void {
  document.body.setAttribute('data-component-id', componentId);
}

export const version : string = '1.9.0.0';
