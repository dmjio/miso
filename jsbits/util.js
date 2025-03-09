window = typeof window === 'undefined' ? {} : window;
window['callFocus'] = function (id) {
  setTimeout(function(){
    var ele = document.getElementById(id);
    if (ele && ele.focus) ele.focus()
  }, 50);
}

window['callBlur'] = function (id) {
  setTimeout(function(){
    var ele = document.getElementById(id);
    if (ele && ele.blur) ele.blur()
  }, 50);
}

window['setBodyComponent'] = function (componentId) {
   document.body.setAttribute('data-component-id', componentId);
}
