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

window['collapseSiblingTextNodes'] = function collapseSiblingTextNodes(vs) {
  if (!vs) { return []; }
  var ax = 0, adjusted = vs.length > 0 ? [vs[0]] : [];
  for (var ix = 1; ix < vs.length; ix++) {
    if (adjusted[ax]['type'] === 'vtext' && vs[ix]['type'] === 'vtext') {
	adjusted[ax]['text'] += ' ' + vs[ix]['text'];
	continue;
    }
    adjusted[++ax] = vs[ix];
  }
  return adjusted;
}
