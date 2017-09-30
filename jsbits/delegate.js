/* event delegation algorithm */
function delegate(events, getVTree) {
    for (var event in events) {
	document.body.addEventListener(events[event][0], function(e) {
	    delegateEvent ( e
			  , getVTree()
			  , buildTargetToBody(document.body, e.target)
			  );
	     }, events[event][1]);
    }
}

/* Accumulate parent stack as well for propagation */
function delegateEvent (event, obj, stack) {
    var idx = 0, node;
    /* sanity check */
    if (obj.domRef === stack[stack.length - 1]) {
      stack[stack.length - 1] = node;
      execute (obj, event, stack);
    }
    stack[idx] = obj;
    idx++;
    for (var o = 0; o < obj.children.length; o++) {
      node = obj.children[o];
      /* skip vtext */
      if (node.type === "vtext") continue;
      /* matched, keep digging */
       if (node.domRef === stack[idx]) {
        /* we found it! */
        if (idx === stack.length - 1) {
          delete stack[idx];
          execute(node, event, stack);
          break;
	}
	idx++;
	obj = node;
	continue;
      }
      /* couldn't find it, exit */
      if (o === obj.children.length - 1) break;
    }
}

function execute (obj, event, parentStack) {
  var eventObj = obj.events[event.type];
  if (eventObj) {
    var options = eventObj.options;
    if (options.preventDefault) event.preventDefault();
    eventObj.runEvent(event);
    if (!options.stopPropagation)
      propogateWhileAble (parentStack, event);
    } else {
      /* still propagate to parent handlers even if event not defined */
      propogateWhileAble (parentStack, event);
    }
}

function buildTargetToBody (body, target) {
    var stack = [];
    while (body !== target) {
      stack.unshift (target);
      target = target.parentNode;
    }
    return stack;
}

function propogateWhileAble (parentStack, event) {
  for (var i = parentStack.length - 1; i > 0; i--) {
    if (parentStack[i] && parentStack[i].events[event.type]) {
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
