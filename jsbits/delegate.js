window['miso'] = window['miso'] || {};

window['miso']['event'] = (function () {

  var oldCallbacks = [];
  var currentCallbacks = [];
  
  /* Callbacks in ghcjs need to be released. With this function one can register
     callbacks that should be released right before diffing.
     */
  var registerCallback = function (cb) {
    currentCallbacks.push(cb);
  };
  
  /* Swaps out the new calbacks for old callbacks.
  The old callbacks should be cleared once the new callbacks have replaced them.
  */
  var swapCallbacks = function() {
    oldCallbacks = currentCallbacks;
    currentCallbacks = [];
  };
  
  /* This releases the old callbacks. */
  var releaseCallbacks = function () {
    for (var i in oldCallbacks) {
      if (h$release) {
        // dmj: figure out what this is for the WASM backend
        h$release(oldCallbacks[i]);
      }
    }
    oldCallbacks = [];
  };
  
  /* event delegation algorithm */
  var delegate = function (mount, events, getVTree) {
    for (var event in events)
      mount.addEventListener
        ( events[event][0]
        , function (e) { listener(e, mount, getVTree); }
        , events[event][1]
        );
  };
  
  var listener = function(e, mount, getVTree) {
     getVTree(function (obj) {
        delegateEvent(e, obj, buildTargetToElement(mount, e.target), []);
     });
  }
  
  /* event delegation algorithm */
  var undelegate = function (mount, events, getVTree) {
    for (var event in events)
      mount.removeEventListener
        ( events[event][0]
        , function (e) { listener(e, mount, getVTree); }
        , events[event][1]
        );
  };
  
  /* Accumulate parent stack as well for propagation */
  var delegateEvent = function (event, obj, stack, parentStack) {
  
    /* base case, not found */
    if (!stack.length) return;
  
    /* stack not length 1, recurse */
    else if (stack.length > 1) {
      parentStack.unshift(obj);
      for (var o = 0; o < obj.children.length; o++) {
        if (obj['type'] === 'vcomp') continue;
        if (obj.children[o]['domRef'] === stack[1]) {
          delegateEvent( event, obj.children[o], stack.slice(1), parentStack );
          break;
        }
      }
    }
  
    /* stack.length == 1 */
    else {
      var eventObj = obj['events'][event.type];
      if (eventObj) {
        var options = eventObj.options;
        if (options['preventDefault'])
          event.preventDefault();
        eventObj['runEvent'](event);
        if (!options['stopPropagation'])
          propagateWhileAble (parentStack, event);
      } else {
        /* still propagate to parent handlers even if event not defined */
        propagateWhileAble (parentStack, event);
      }
    }
  };
  
  var buildTargetToElement = function (element, target) {
    var stack = [];
    while (element !== target) {
      stack.unshift (target);
      target = target.parentNode;
    }
    return stack;
  };
  
  var propagateWhileAble = function (parentStack, event) {
    for (var i = 0; i < parentStack.length; i++) {
      if (parentStack[i]['events'][event.type]) {
        var eventObj = parentStack[i]['events'][event.type], options = eventObj['options'];
        if (options['preventDefault']) event.preventDefault();
        eventObj['runEvent'](event);
        if (options['stopPropagation']) {
          event.stopPropagation();
          break;
        }
      }
    }
  };
  
  /* Walks down obj following the path described by `at`, then filters primitive
   values (string, numbers and booleans)*/
  var objectToJSON = function (at, obj) {
    /* If at is of type [[MisoString]] */
    if (typeof at[0] == 'object') {
      var ret = [];
      for (var i = 0; i < at.length; i++)
        ret.push(window['objectToJSON'](at[i], obj));
      return ret;
    }
  
    for (var i in at) obj = obj[at[i]];
  
    /* If obj is a list-like object */
    var newObj;
    if (obj instanceof Array || ('length' in obj && obj['localName'] !== 'select')) {
      newObj = [];
      for (var i = 0; i < obj.length; i++)
        newObj.push(window['objectToJSON']([], obj[i]));
      return newObj;
    }
  
    /* If obj is a non-list-like object */
    newObj = {};
    for (var i in getAllPropertyNames(obj)){
      /* bug in safari, throws TypeError if the following fields are referenced on a checkbox */
      /* https://stackoverflow.com/a/25569117/453261 */
      /* https://html.spec.whatwg.org/multipage/input.html#do-not-apply */
      if ((obj['localName'] === 'input') && (i === 'selectionDirection' || i === 'selectionStart' || i === 'selectionEnd'))
        continue;
      if (typeof obj[i] == 'string' || typeof obj[i] == 'number' || typeof obj[i] == 'boolean')
        newObj[i] = obj[i];
    }
    return newObj;
  };
  
  /* get static and dynamic properties */
  function getAllPropertyNames(obj) {
    var props = {}, i = 0;
    do {
      var names = Object.getOwnPropertyNames(obj);
      for (i = 0; i < names.length; i++) {
        props [names[i]] = null;
      }
    } while (obj = Object.getPrototypeOf(obj));
    return props;
  };

  return {
    'delegate' : delegate,
    'undelegate' : undelegate
  };

})()
