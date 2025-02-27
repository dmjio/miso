window['miso'] = window['miso'] || {};

window['miso']['utils'] = (function() {

    var callFocus = function(id) {
        setTimeout(function(){
            var ele = document.getElementById(id);
            if (ele && ele.focus) ele.focus()
        }, 50);
    };

    var callBlur = function (id) {
      setTimeout(function(){
        var ele = document.getElementById(id);
        if (ele && ele.blur) ele.blur()
      }, 50);
    };

    return {
      'callFocus' : callFocus,
      'callBlur' : callBlur
    };

})();
