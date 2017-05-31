function callFocus(id) {
    var ele = document.getElementById(id);
    if (ele && ele.focus) ele.focus()
}

function callBlur(id) {
    var ele = document.getElementById(id);
    if (ele && ele.blur) ele.blur()
}
