/* event delegation algorithm */
function delegate(events, getVTree, writer) {
    for (var event in events) {
	document.body.addEventListener(events[event][0], function(e) {
            delegateEvent ( e
                          , getVTree()
                          , writer
                          , buildTargetToBody(document.body, e.target, []).reverse()
                          , []
                          );
	}, events[event][1]);
    }
}

/ * Accumulate parent stack as well for propogation */
function delegateEvent (event, obj, writer, stack, parentStack) {

    /* base case, not found */
    if (!stack.length) return;

    /* stack not length 1, recurse */
    else if (stack.length > 1) {
      if (obj.domRef === stack[0]) parentStack.push(obj);
	for (var o = 0; o < obj.children.length; o++) {
          if (obj.children[o].type === "vtext") continue;
          delegateEvent ( event
                        , obj.children[o]
                        , writer
			, stack.slice(1)
                        , parentStack
			);
       }
    }

    /* stack.length == 1 */
    else {
	if (obj.domRef === stack[0]) {
          if (obj.events[event.type]) {
	      var eventObj = obj.events[event.type],
		  options = eventObj.options;
            if (options.preventDefault) event.preventDefault();
            writer(eventObj.runEvent(event));
	    if (!options.stopPropagation)
	     propogateWhileAble (parentStack.reverse(), event, writer);
          }
	}
    }
}


function buildTargetToBody (body, target, stack) {
    while (body !== target) {
      stack.push (target);
      target = target.parentNode;
    }
    return stack;
}

function propogateWhileAble (parentStack, event, writer) {
  for (var i = 0; i < parentStack.length; i++) {
    if (parentStack[i].events[event.type]) {
      var eventObj = parentStack[i].events[event.type],
          options = eventObj.options;
        if (options.preventDefault) event.preventDefault();
        writer(eventObj.runEvent(event));
  	if (options.stopPropagation) break;
    }
  }
}
