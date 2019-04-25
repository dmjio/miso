window.oldCallbacks = [];
window.currentCallbacks = [];

/* Callbacks in ghcjs need to be released. With this function one can register
   callbacks that should be released right before diffing.
*/
window.registerCallback = function registerCallback(cb) {
  currentCallbacks.push(cb);
};

/* Swaps out the new calbacks for old callbacks.
The old callbacks should be cleared once the new callbacks have replaced them.
*/
window.swapCallbacks = function swapCallbacks() {
  oldCallbacks = currentCallbacks;
  currentCallbacks = [];
};

/* This releases the old callbacks. */
window.releaseCallbacks = function releaseCallbacks() {
  for (var i in oldCallbacks)
    h$release(oldCallbacks[i]);

  oldCallbacks = [];
};

/* event delegation algorithm */
window.delegate = function delegate(mountPointElement, events, getVTree) {
  for (var event in events) {
    mountPointElement.addEventListener(events[event][0], function(e) {
      getVTree(function (obj) {
        delegateEvent ( e
                        , obj
                        , buildTargetToElement(mountPointElement, e.target)
                        , []
                      );
      });
    }, events[event][1]);
  }
};

/* Accumulate parent stack as well for propagation */
window.delegateEvent = function delegateEvent (event, obj, stack, parentStack) {

  /* base case, not found */
  if (!stack.length) return;

  /* stack not length 1, recurse */
  else if (stack.length > 1) {
    if (obj.domRef === stack[0]) parentStack.unshift(obj);
    for (var o = 0; o < obj.children.length; o++) {
      if (obj.children[o].type === "vtext") continue;
      delegateEvent ( event
                    , obj.children[o]
                    , stack.slice(1)
                    , parentStack
                    );
     }
  }

  /* stack.length == 1 */
  else {
    if (obj.domRef === stack[0]) {
      var eventObj = obj.events[event.type];
      if (eventObj) {
        var options = eventObj.options;
      if (options.preventDefault)
        event.preventDefault();
      eventObj.runEvent(event);
      if (!options.stopPropagation)
        propogateWhileAble (parentStack, event);
      } else {
        /* still propagate to parent handlers even if event not defined */
        propogateWhileAble (parentStack, event);
      }
    }
  }
};

window.buildTargetToElement = function buildTargetToElement (element, target) {
  var stack = [];
  while (element !== target) {
    stack.unshift (target);
    target = target.parentNode;
  }
  return stack;
};

window.propogateWhileAble = function propogateWhileAble (parentStack, event) {
  for (var i = 0; i < parentStack.length; i++) {
    if (parentStack[i].events[event.type]) {
      var eventObj = parentStack[i].events[event.type],
          options = eventObj.options;
      if (options.preventDefault) event.preventDefault();
      eventObj.runEvent(event);
      if (options.stopPropagation) break;
    }
  }
};

/* Walks down obj following the path described by `at`, then filters primitive
 values (string, numbers and booleans)*/
window.objectToJSON = function objectToJSON (at, obj) {
  /* If at is of type [[MisoString]] */
  if (typeof at[0] == "object") {
    var ret = [];
    for (var i = 0; i < at.length; i++)
      ret.push(objectToJSON(at[i], obj));
    return (ret);
  }

  for (var i in at) obj = obj[at[i]];

  /* If obj is a list-like object */
  if (obj instanceof Array) {
    var newObj = [];
    for (var i = 0; i < obj.length; i++)
      newObj.push(objectToJSON([], obj[i]));
    return (newObj);
  }

  /* If obj is a non-list-like object */
  var newObj = {};
  for (var i in obj){
    /* bug in safari, throws TypeError if the following fields are referenced on a checkbox */
    /* https://stackoverflow.com/a/25569117/453261 */
    /* https://html.spec.whatwg.org/multipage/input.html#do-not-apply */
    if (obj['type'] == "checkbox" && (i === "selectionDirection" || i === "selectionStart" || i === "selectionEnd"))
      continue;
    if (typeof obj[i] == "string" || typeof obj[i] == "number" || typeof obj[i] == "boolean")
      newObj[i] = obj[i];
  }
  return newObj;
};
