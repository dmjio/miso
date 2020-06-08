window = typeof window === 'undefined' ? {} : window;
window['callFocus'] = function callFocus(id) {
  setTimeout(function(){
    var ele = document.getElementById(id);
    if (ele && ele.focus) ele.focus()
  }, 50);
}

window['callBlur'] = function callBlur(id) {
  setTimeout(function(){
    var ele = document.getElementById(id);
    if (ele && ele.blur) ele.blur()
  }, 50);
}
