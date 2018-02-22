"use strict"; // Both performance and bug improvements, OK: https://caniuse.com/#search=strict%20mode

/**
 * CODE STYLE NOTES
 * ===============
 *
 * const is preferred b/c it's safer, optimizable, widely supported: https://caniuse.com/#search=const
 * [].forEach/filter/map is OK (it's part of ES5): https://caniuse.com/#search=ECMAScript%205%20Strict%20Mode
 */



/* event delegation algorithm */
function delegate(mountPointElement, events, getVTree) {
  const vtree = getVTree(); // This *is* a constant relative to all the events, correct?
  events.forEach(function(event) {
    mountPointElement.addEventListener(events[event][0], function(e) {
      // NB: Sorely tempted to do a window.setTimeout here as an ad hoc trampoline.
      // We should check performance of this implementation vs. that one. If the difference is trivial,
      // it's worth trampolining to clear the stack, let other things run, etc., etc.
      // If we do this, buildTargetToElement, events[event][0], events[event][1] should all
      // be called and assigned to const values *before* trampolining.
      delegateEvent ( e
                    , vtree
                    , buildTargetToElement(mountPointElement, e.target)
                    , []
                    );
    }, events[event][1]);
  });
}

/* Accumulate parent stack as well for propagation */
function delegateEvent (event, obj, stack, parentStack) {
    const stackLength = stack && stack.length;

    if (!stackLength) { /* subbase case, not found */
      return;
    } else if (stackLength > 1) { /* stack not length 1, recurse */
      if (obj.domRef === stack[0]) parentStack.unshift(obj); // Do we mean for this to change things globally?
      const stackSlice = stack.slice(1);
      obj.children.filter(function(child) {
        return (child.type /= "vtext");
      }).forEach(function(child) {
        // We could potentially trampoline here, as well. The other spot is better, and runs less risk of
        // mucking up expected intuitive execution order.
        delegateEvent(event, child, stackSlice, parentStack);
      });
    } else if(stackLength == 1) { /* stack.length == 1, base case */
        if (obj.domRef === stack[0]) {
            const eventObj = obj.events[event.type];
            if (eventObj) {
                const options = eventObj.options;
                if (options.preventDefault) {
                  event.preventDefault();
                }
                eventObj.runEvent(event);
                if (!options.stopPropagation) {
                  propogateWhileAble (parentStack, event);
                }
            } else {
               /* still propagate to parent handlers even if event not defined */
               propogateWhileAble (parentStack, event);
            }
        } else {
          // What does this case mean? Do we really want to silently ignore it?
        }
    } else {
      throw new Error("unexpected stack.length: " + stackLength);
    }
}

function buildTargetToElement (element, target) {
    const stack = [];
    while (target && element !== target) {
      stack.unshift (target);
      target = target.parentNode;
    }
    return stack;
}

function propogateWhileAble (parentStack, event) {
  const eventType = event.type;
  // ES5 doesn't have fold. :(
  for (var i = 0; i < parentStack.length; i++) { // const is dodgy in this position
    const parentFrame = parentStack[i];
    if (parentFrame.events[eventType]) {
      const eventObj = parentFrame.events[eventType];
      const options = eventObj.options;
      if (options.preventDefault) event.preventDefault();
      eventObj.runEvent(event);
      if (options.stopPropagation) break; // Have to use a loop b/c of this logic
    }
  }
}

/* Walks down obj following the path described by `at`, then filters primitive
 values (string, numbers and booleans)*/
function objectToJSON (at, obj) {
  /* If at is of type [[MisoString]] */
  if (typeof at[0] == "object") {
    const ret = [];
    for(var i = 0; i < at.length; i++) {
      ret.push(objectToJSON(at[i], obj));
    }
    return ret;
  }

  // What is this doing???
  for (var i in at) obj = obj[at[i]];

  /* If obj is a list-like object */
  if (obj.forEach) {
    const newObj = [];
    obj.forEach(function(it) {
      newObj.push(objectToJSON([], it));
    });
    return (newObj);
  }

  /* If obj is a non-list-like object */
  const newObj = {};
  for (var i in obj) {
    const objI = obj[i];
    switch(typeof objI) {
      case "string":
      case "number":
      case "boolean":
        newObj[i] = objI;
        break;
      default:
        // Do nothing
    }
  }
  return (newObj);
}
