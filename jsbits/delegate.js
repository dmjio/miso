/* event delegation algorithm */
function delegate(mountPointElement, events, getVTree) {
    for (var event in events) {
	mountPointElement.addEventListener(events[event][0], function(e) {
            delegateEvent ( e
                          , getVTree()
                          , buildTargetToElement(mountPointElement, e.target)
                          , []
                          );
	     }, events[event][1]);
    }
}

/* Accumulate parent stack as well for propagation */
function delegateEvent (event, obj, stack, parentStack) {

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
}

function buildTargetToElement (element, target) {
    var stack = [];
    while (element !== target) {
      stack.unshift (target);
      target = target.parentNode;
    }
    return stack;
}

function propogateWhileAble (parentStack, event) {
  for (var i = 0; i < parentStack.length; i++) {
    if (parentStack[i].events[event.type]) {
      var eventObj = parentStack[i].events[event.type],
          options = eventObj.options;
        if (options.preventDefault) event.preventDefault();
        eventObj.runEvent(event);
  	if (options.stopPropagation) break;
    }
  }
}

/* Convert event to JSON at a specific location in the DOM tree*/
function objectToJSON (at, obj) {
    for (var i in at) obj = obj[at[i]];
    var newObj = {};
    for (var i in obj)
	if (typeof obj[i] == "string" || typeof obj[i] == "number" || typeof obj[i] == "boolean")
	    newObj[i] = obj[i];
    return (newObj);
}
