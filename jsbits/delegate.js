/* event delegation algorithm */
window['delegate'] = function (mount, events, getVTree, debug) {
  for (var event in events)
    mount.addEventListener
      ( events[event][0]
      , function (e) { window['listener'](e, mount, getVTree, debug); }
      , events[event][1]
      );
};

/* the event listener shared by both delegator and undelegator */
window['listener'] = function(e, mount, getVTree, debug) {
    getVTree(function (obj) {
    	if (e.target) {
    	    window['delegateEvent'](e, obj, window['buildTargetToElement'](mount, e.target), [], debug);
    	}
   });
}

/* event undelegation */
window['undelegate'] = function (mount, events, getVTree, debug) {
  for (var event in events)
    mount.removeEventListener
      ( events[event][0]
      , function (e) { window['listener'](e, mount, getVTree, debug); }
      , events[event][1]
      );
};

/* Finds event in virtual dom via pointer equality
   Accumulate parent stack as well for propagation up the vtree
 */
window['delegateEvent'] = function (event, obj, stack, parentStack, debug) {

  /* base case, not found */
  if (!stack.length) {
     if (debug) {
       console.warn('Event "' + event.type + '" did not find an event handler to dispatch on', obj, event);
     }
     return;
  }

  /* stack not length 1, recurse */
  else if (stack.length > 1) {
    parentStack.unshift(obj);
    for (var o = 0; o < obj.children.length; o++) {
      if (obj['type'] === 'vcomp') continue;
      if (obj.children[o]['domRef'] === stack[1]) {
        window['delegateEvent'](event, obj.children[o], stack.slice(1), parentStack, debug);
        break;
      }
    }
  }

  /* stack.length == 1 */
  else {
    var eventObj = obj['events'][event.type];
    if (eventObj) {
      var options = eventObj['options'];
      if (options['preventDefault'])
        event.preventDefault();
      eventObj['runEvent'](event);
      if (!options['stopPropagation'])
        window['propagateWhileAble'] (parentStack, event);
    } else {
      /* still propagate to parent handlers even if event not defined */
      window['propagateWhileAble'] (parentStack, event);
    }
  }
};

/* Create a stack of ancestors used to index into the virtual DOM */
window['buildTargetToElement'] = function buildTargetToElement (element, target) {
  var stack = [];
  while (element !== target) {
    stack.unshift (target);
    target = target.parentNode;
  }
  return stack;
};

/* Propagate the event up the chain, invoking other event handlers as encountered */
window['propagateWhileAble'] = function propagateWhileAble (parentStack, event) {
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
   values (string, numbers and booleans). Sort of like JSON.stringify(), but
   on an Event that is stripped of impure references.
*/
window['eventJSON'] = function eventJSON (at, obj) {
  /* If at is of type [[MisoString]] */
  if (typeof at[0] == 'object') {
    var ret = [];
    for (var i = 0; i < at.length; i++)
      ret.push(window['eventJSON'](at[i], obj));
    return ret;
  }

  for (var i in at) obj = obj[at[i]];

  /* If obj is a list-like object */
  var newObj;
  if (obj instanceof Array || ('length' in obj && obj['localName'] !== 'select')) {
    newObj = [];
    for (var i = 0; i < obj.length; i++)
      newObj.push(window['eventJSON']([], obj[i]));
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
