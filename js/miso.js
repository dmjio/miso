// ts/miso/util.ts
var version = "1.9.0.0";
function callFocus(id, delay) {
  var setFocus = function() {
    var e = document.getElementById(id);
    if (e && e.focus)
      e.focus();
  };
  delay > 0 ? setTimeout(setFocus, delay) : setFocus();
}
function callBlur(id, delay) {
  var setBlur = function() {
    var e = document.getElementById(id);
    if (e && e.blur)
      e.blur();
  };
  delay > 0 ? setTimeout(setBlur, delay) : setBlur();
}
function callSelect(id, delay) {
  var setSelect = function() {
    var e = document.getElementById(id);
    if (e && typeof e["select"] === "function")
      e.select();
  };
  delay > 0 ? setTimeout(setSelect, delay) : setSelect();
}
function callSetSelectionRange(id, start, end, delay) {
  var setSetSelectionRange = function() {
    var e = document.getElementById(id);
    if (e && typeof e["setSelectionRange"] === "function")
      e.setSelectionRange(start, end, "none");
  };
  delay > 0 ? setTimeout(setSetSelectionRange, delay) : setSetSelectionRange();
}
function fetchCore(url, method, body, requestHeaders, successful, errorful, responseType) {
  var options = { method, headers: requestHeaders };
  if (body) {
    options["body"] = body;
  }
  let headers = {};
  let status = null;
  try {
    fetch(url, options).then((response) => {
      status = response.status;
      for (const [key, value] of response.headers) {
        headers[key] = value;
      }
      if (!response.ok) {
        throw new Error(response.statusText);
      }
      if (responseType == "json") {
        return response.json();
      } else if (responseType == "text") {
        return response.text();
      } else if (responseType === "arrayBuffer") {
        return response.arrayBuffer();
      } else if (responseType === "blob") {
        return response.blob();
      } else if (responseType === "bytes") {
        return response.bytes();
      } else if (responseType === "formData") {
        return response.formData();
      } else if (responseType === "none") {
        return successful({ error: null, body: null, headers, status });
      }
    }).then((body2) => successful({ error: null, body: body2, headers, status })).catch((body2) => errorful({ error: null, body: body2, headers, status }));
  } catch (err) {
    errorful({ body: null, error: err.message, headers, status });
  }
}
function websocketConnect(url, onOpen, onClose, onMessageText, onMessageJSON, onMessageBLOB, onMessageArrayBuffer, onError, textOnly) {
  try {
    let socket = new WebSocket(url);
    socket.onopen = function() {
      onOpen();
    };
    socket.onclose = function(e) {
      onClose(e);
    };
    socket.onerror = function(error) {
      console.error(error);
      onError("WebSocket error received");
    };
    socket.onmessage = function(msg) {
      if (typeof msg.data === "string") {
        try {
          if (textOnly) {
            if (onMessageText)
              onMessageText(msg.data);
            return;
          }
          const json = JSON.parse(msg.data);
          if (onMessageJSON)
            onMessageJSON(json);
        } catch (err) {
          if (textOnly && onMessageText) {
            onMessageText(msg.data);
          } else {
            onError(err.message);
          }
        }
      } else if (msg.data instanceof Blob) {
        if (onMessageBLOB)
          onMessageBLOB(msg.data);
      } else if (msg.data instanceof ArrayBuffer) {
        if (onMessageArrayBuffer)
          onMessageArrayBuffer(msg.data);
      } else {
        console.error("Received unknown message type from WebSocket", msg);
        onError("Unknown message received from WebSocket");
      }
    };
    return socket;
  } catch (err) {
    onError(err.message);
  }
}
function websocketClose(socket) {
  if (socket) {
    socket.close();
    socket = null;
  }
}
function websocketSend(socket, message) {
  if (message && socket && socket.readyState === WebSocket.OPEN) {
    socket.send(message);
  }
}
function eventSourceConnect(url, onOpen, onMessageText, onMessageJSON, onError, textOnly) {
  try {
    let eventSource = new EventSource(url);
    eventSource.onopen = function() {
      onOpen();
    };
    eventSource.onerror = function() {
      onError("EventSource error received");
    };
    eventSource.onmessage = function(msg) {
      try {
        if (textOnly) {
          if (onMessageText)
            onMessageText(msg.data);
          return;
        }
        const json = JSON.parse(msg.data);
        if (onMessageJSON)
          onMessageJSON(json);
      } catch (err) {
        if (textOnly && onMessageText) {
          onMessageText(msg.data);
        } else {
          onError(err.message);
        }
      }
    };
    return eventSource;
  } catch (err) {
    onError(err.message);
  }
}
function eventSourceClose(eventSource) {
  if (eventSource) {
    eventSource.close();
    eventSource = null;
  }
}
function populateClass(vnode, classes) {
  if (!vnode.classList) {
    vnode.classList = new Set;
  }
  for (const str of classes) {
    for (const c of str.trim().split(" ")) {
      if (c)
        vnode.classList.add(c);
    }
  }
}
function updateRef(current, latest) {
  if (!current.parent) {
    return;
  }
  latest.nextSibling = current.nextSibling;
  latest.parent = current.parent;
  current.parent.child = latest;
}
function inline(code, context = {}) {
  const keys = Object.keys(context);
  const values = Object.values(context);
  const func = new Function(...keys, code);
  return func(...values);
}
function typeOf(x) {
  if (x === null || x === undefined)
    return 0;
  if (typeof x === "number")
    return 1;
  if (typeof x === "string")
    return 2;
  if (typeof x === "boolean")
    return 3;
  if (Array.isArray(x))
    return 4;
  return 5;
}
function splitmix32(a) {
  return function() {
    a |= 0;
    a = a + 2654435769 | 0;
    var t = a ^ a >>> 15;
    t = Math.imul(t, 2246822507);
    t = t ^ t >>> 13;
    t = Math.imul(t, 3266489909);
    return ((t ^ t >>> 16) >>> 0) / 4294967296;
  };
}
function getRandomValues() {
  const array = new Uint32Array(1);
  return crypto.getRandomValues(array)[0];
}
function mathRandom() {
  return Math.random();
}
function forEachDOMRef(tree, cb) {
  switch (tree.type) {
    case 3 /* VFrag */:
      for (const child of tree.children)
        forEachDOMRef(child, cb);
      break;
    case 0 /* VComp */:
      if (tree.child)
        forEachDOMRef(tree.child, cb);
      break;
    default:
      cb(tree.domRef);
      break;
  }
}
function getFirstDOMRef(tree) {
  switch (tree.type) {
    case 3 /* VFrag */: {
      if (!tree.children || tree.children.length === 0)
        throw new Error("getFirstDOMRef called on empty VFrag");
      return getFirstDOMRef(tree.children[0]);
    }
    case 0 /* VComp */:
      if (!tree.child)
        throw new Error("getFirstDOMRef called on unmounted VComp");
      return getFirstDOMRef(tree.child);
    default:
      return tree.domRef;
  }
}
function getLastDOMRef(tree) {
  switch (tree.type) {
    case 3 /* VFrag */: {
      if (!tree.children || tree.children.length === 0)
        throw new Error("getLastDOMRef called on empty VFrag");
      return getLastDOMRef(tree.children[tree.children.length - 1]);
    }
    case 0 /* VComp */:
      if (!tree.child)
        throw new Error("getLastDOMRef called on unmounted VComp");
      return getLastDOMRef(tree.child);
    default:
      return tree.domRef;
  }
}

// ts/miso/dom.ts
function diff(c, n, parent, context) {
  if (!c && !n)
    return;
  else if (!c)
    create(n, parent, context);
  else if (!n)
    destroy(c, parent, context);
  else if (c.type === 2 /* VText */ && n.type === 2 /* VText */) {
    diffVText(c, n, context);
  } else if (c.type === 0 /* VComp */ && n.type === 0 /* VComp */) {
    if (n.key === c.key) {
      n.child = c.child;
      n.componentId = c.componentId;
      if (c.child)
        c.child.parent = n;
      return;
    }
    replace(c, n, parent, context);
  } else if (c.type === 3 /* VFrag */ && n.type === 3 /* VFrag */) {
    if (n.key === c.key) {
      const endAnchor = c.children.length > 0 ? getLastDOMRef(c).nextSibling : null;
      diffChildren(c.children, n.children, parent, context, endAnchor);
    } else {
      replace(c, n, parent, context);
    }
  } else if (c.type === 1 /* VNode */ && n.type === 1 /* VNode */) {
    if (n.tag === c.tag && n.key === c.key) {
      n.domRef = c.domRef;
      diffAttrs(c, n, context);
    } else {
      replace(c, n, parent, context);
    }
  } else
    replace(c, n, parent, context);
}
function diffVText(c, n, context) {
  if (c.text !== n.text)
    context.setTextContent(c.domRef, n.text);
  n.domRef = c.domRef;
  return;
}
function replace(c, n, parent, context) {
  if (c.type === 3 /* VFrag */) {
    const anchor = c.children.length > 0 ? getLastDOMRef(c).nextSibling : null;
    destroy(c, parent, context);
    if (anchor) {
      createElement(parent, 2 /* INSERT_BEFORE */, anchor, n, context);
    } else {
      create(n, parent, context);
    }
    return;
  }
  switch (c.type) {
    case 2 /* VText */:
      break;
    default:
      callBeforeDestroyedRecursive(c);
      break;
  }
  const firstRef = getFirstDOMRef(c);
  const lastRef = getLastDOMRef(c);
  if (firstRef !== lastRef) {
    const anchor = lastRef.nextSibling;
    forEachDOMRef(c, (ref) => context.removeChild(parent, ref));
    if (anchor) {
      createElement(parent, 2 /* INSERT_BEFORE */, anchor, n, context);
    } else {
      create(n, parent, context);
    }
  } else {
    createElement(parent, 1 /* REPLACE */, firstRef, n, context);
  }
  switch (c.type) {
    case 2 /* VText */:
      break;
    default:
      callDestroyedRecursive(c);
      break;
  }
}
function destroy(c, parent, context) {
  switch (c.type) {
    case 2 /* VText */:
      break;
    case 3 /* VFrag */:
      for (const child of c.children)
        destroy(child, parent, context);
      return;
    default:
      callBeforeDestroyedRecursive(c);
      break;
  }
  forEachDOMRef(c, (ref) => context.removeChild(parent, ref));
  switch (c.type) {
    case 2 /* VText */:
      break;
    default:
      callDestroyedRecursive(c);
      break;
  }
}
function callDestroyedRecursive(c) {
  if (c.type === 3 /* VFrag */) {
    for (const child of c.children)
      if (child.type !== 2 /* VText */)
        callDestroyedRecursive(child);
    return;
  }
  callDestroyed(c);
  switch (c.type) {
    case 1 /* VNode */:
      for (const child of c.children)
        if (child.type !== 2 /* VText */)
          callDestroyedRecursive(child);
      break;
    case 0 /* VComp */:
      if (c.child && c.child.type !== 2 /* VText */)
        callDestroyedRecursive(c.child);
      break;
  }
}
function callDestroyed(c) {
  if (c.type === 1 /* VNode */ && c.onDestroyed)
    c.onDestroyed();
  if (c.type === 0 /* VComp */)
    unmountComponent(c);
}
function callBeforeDestroyed(c) {
  switch (c.type) {
    case 0 /* VComp */:
      break;
    case 1 /* VNode */:
      if (c.onBeforeDestroyed)
        c.onBeforeDestroyed();
      break;
    default:
      break;
  }
}
function callBeforeDestroyedRecursive(c) {
  if (c.type === 3 /* VFrag */) {
    for (const child of c.children)
      if (child.type !== 2 /* VText */)
        callBeforeDestroyedRecursive(child);
    return;
  }
  callBeforeDestroyed(c);
  switch (c.type) {
    case 1 /* VNode */:
      for (const child of c.children) {
        if (child.type === 2 /* VText */)
          continue;
        callBeforeDestroyedRecursive(child);
      }
      break;
    case 0 /* VComp */:
      if (c.child && c.child.type !== 2 /* VText */)
        callBeforeDestroyedRecursive(c.child);
      break;
  }
}
function diffAttrs(c, n, context) {
  diffProps(c ? c.props : {}, n.props, n.domRef, n.ns === "svg", context);
  diffClass(c ? c.classList : null, n.classList, n.domRef, context);
  diffCss(c ? c.css : {}, n.css, n.domRef, context);
  diffChildren(c ? c.children : [], n.children, n.domRef, context);
  drawCanvas(n);
}
function diffClass(c, n, domRef, context) {
  if (!c && !n) {
    return;
  }
  if (!c) {
    for (const className of n) {
      context.addClass(className, domRef);
    }
    return;
  }
  if (!n) {
    for (const className of c) {
      context.removeClass(className, domRef);
    }
    return;
  }
  for (const className of c) {
    if (!n.has(className)) {
      context.removeClass(className, domRef);
    }
  }
  for (const className of n) {
    if (!c.has(className)) {
      context.addClass(className, domRef);
    }
  }
  return;
}
function diffProps(cProps, nProps, node, isSvg, context) {
  var newProp;
  for (const c in cProps) {
    newProp = nProps[c];
    if (newProp === undefined) {
      if (isSvg || !(c in node) || c === "disabled") {
        context.removeAttribute(node, c);
      } else {
        context.setAttribute(node, c, "");
      }
    } else {
      if (newProp === cProps[c] && c !== "checked" && c !== "value")
        continue;
      if (isSvg) {
        if (c === "href") {
          context.setAttributeNS(node, "http://www.w3.org/1999/xlink", "href", newProp);
        } else {
          context.setAttribute(node, c, newProp);
        }
      } else if (c in node && !(c === "list" || c === "form")) {
        node[c] = newProp;
      } else {
        context.setAttribute(node, c, newProp);
      }
    }
  }
  for (const n in nProps) {
    if (cProps && n in cProps)
      continue;
    newProp = nProps[n];
    if (isSvg) {
      if (n === "href") {
        context.setAttributeNS(node, "http://www.w3.org/1999/xlink", "href", newProp);
      } else {
        context.setAttribute(node, n, newProp);
      }
    } else if (n in node && !(n === "list" || n === "form")) {
      node[n] = nProps[n];
    } else {
      context.setAttribute(node, n, newProp);
    }
  }
}
function diffCss(cCss, nCss, node, context) {
  context.setInlineStyle(cCss, nCss, node);
}
function shouldSync(cs, ns) {
  if (cs.length === 0 || ns.length === 0)
    return false;
  for (var i = 0;i < cs.length; i++) {
    if (cs[i].key === null || cs[i].key === undefined) {
      return false;
    }
  }
  for (var i = 0;i < ns.length; i++) {
    if (ns[i].key === null || ns[i].key === undefined) {
      return false;
    }
  }
  return true;
}
function diffChildren(cs, ns, parent, context, endAnchor = null) {
  if (shouldSync(cs, ns)) {
    syncChildren(cs, ns, parent, context, endAnchor);
  } else {
    for (let i = 0;i < Math.max(ns.length, cs.length); i++) {
      const c = cs[i], n = ns[i];
      if (!c && n) {
        if (endAnchor) {
          createElement(parent, 2 /* INSERT_BEFORE */, endAnchor, n, context);
        } else {
          create(n, parent, context);
        }
      } else {
        diff(c, n, parent, context);
      }
    }
  }
}
function populateDomRef(c, context) {
  if (c.ns === "svg") {
    c.domRef = context.createElementNS("http://www.w3.org/2000/svg", c.tag);
  } else if (c.ns === "mathml") {
    c.domRef = context.createElementNS("http://www.w3.org/1998/Math/MathML", c.tag);
  } else {
    c.domRef = context.createElement(c.tag);
  }
}
function callCreated(parent, n, context) {
  if (n.onCreated)
    n.onCreated(n.domRef);
}
function createElement(parent, op, replacing, n, context) {
  switch (n.type) {
    case 2 /* VText */:
      n.domRef = context.createTextNode(n.text);
      switch (op) {
        case 2 /* INSERT_BEFORE */:
          context.insertBefore(parent, n.domRef, replacing);
          break;
        case 0 /* APPEND */:
          context.appendChild(parent, n.domRef);
          break;
        case 1 /* REPLACE */:
          context.replaceChild(parent, n.domRef, replacing);
          break;
      }
      break;
    case 3 /* VFrag */:
      for (const child of n.children) {
        createElement(parent, 2 /* INSERT_BEFORE */, replacing, child, context);
      }
      if (op === 1 /* REPLACE */ && replacing) {
        context.removeChild(parent, replacing);
      }
      break;
    case 0 /* VComp */:
      mountComponent(parent, op, replacing, n, context);
      break;
    case 1 /* VNode */:
      if (n.onBeforeCreated)
        n.onBeforeCreated();
      populateDomRef(n, context);
      if (n.onCreated)
        n.onCreated(n.domRef);
      diffAttrs(null, n, context);
      switch (op) {
        case 2 /* INSERT_BEFORE */:
          context.insertBefore(parent, n.domRef, replacing);
          break;
        case 0 /* APPEND */:
          context.appendChild(parent, n.domRef);
          break;
        case 1 /* REPLACE */:
          context.replaceChild(parent, n.domRef, replacing);
          break;
      }
      break;
  }
}
function drawCanvas(c) {
  if (c.tag === "canvas" && c.draw)
    c.draw(c.domRef);
}
function unmountComponent(c) {
  c.unmount(c.componentId);
}
function mountComponent(parent, op, replacing, n, context) {
  let mounted = n.mount(parent);
  n.componentId = mounted.componentId;
  n.child = mounted.componentTree;
  mounted.componentTree.parent = n;
  if (mounted.componentTree.type !== 0 /* VComp */) {
    if (op === 1 /* REPLACE */ && replacing) {
      if (mounted.componentTree.type === 3 /* VFrag */) {
        forEachDOMRef(mounted.componentTree, (ref) => context.insertBefore(parent, ref, replacing));
        context.removeChild(parent, replacing);
      } else {
        context.replaceChild(parent, getFirstDOMRef(mounted.componentTree), replacing);
      }
    } else if (op === 2 /* INSERT_BEFORE */) {
      forEachDOMRef(mounted.componentTree, (ref) => context.insertBefore(parent, ref, replacing));
    }
  }
}
function create(n, parent, context) {
  createElement(parent, 0 /* APPEND */, null, n, context);
}
function insertBefore(parent, n, o, context) {
  const anchor = o ? getFirstDOMRef(o) : null;
  forEachDOMRef(n, (ref) => context.insertBefore(parent, ref, anchor));
}
function swapDOMRef(oLast, oFirst, parent, context) {
  if ((oLast.type === 1 /* VNode */ || oLast.type === 2 /* VText */) && (oFirst.type === 1 /* VNode */ || oFirst.type === 2 /* VText */)) {
    context.swapDOMRefs(getFirstDOMRef(oLast), getFirstDOMRef(oFirst), parent);
    return;
  }
  const tmp = getLastDOMRef(oLast).nextSibling;
  const anchor = getFirstDOMRef(oFirst);
  forEachDOMRef(oLast, (ref) => context.insertBefore(parent, ref, anchor));
  forEachDOMRef(oFirst, (ref) => context.insertBefore(parent, ref, tmp));
}
function syncChildren(os, ns, parent, context, endAnchor = null) {
  var oldFirstIndex = 0, newFirstIndex = 0, oldLastIndex = os.length - 1, newLastIndex = ns.length - 1, tmp, nFirst, nLast, oLast, oFirst, found, node;
  for (;; ) {
    if (newFirstIndex > newLastIndex && oldFirstIndex > oldLastIndex) {
      break;
    }
    nFirst = ns[newFirstIndex];
    nLast = ns[newLastIndex];
    oFirst = os[oldFirstIndex];
    oLast = os[oldLastIndex];
    if (oldFirstIndex > oldLastIndex) {
      if (endAnchor) {
        createElement(parent, 2 /* INSERT_BEFORE */, endAnchor, nFirst, context);
      } else {
        diff(null, nFirst, parent, context);
        insertBefore(parent, nFirst, oFirst, context);
      }
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    } else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        destroy(os[oldLastIndex--], parent, context);
      }
      os.splice(oldFirstIndex, tmp - oldFirstIndex + 1);
      break;
    } else if (oFirst.key === nFirst.key) {
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent, context);
    } else if (oLast.key === nLast.key) {
      diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
    } else if (oFirst.key === nLast.key && nFirst.key === oLast.key) {
      swapDOMRef(oLast, oFirst, parent, context);
      swap(os, oldFirstIndex, oldLastIndex);
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent, context);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
    } else if (oFirst.key === nLast.key) {
      const afterOLast = getLastDOMRef(oLast).nextSibling;
      forEachDOMRef(oFirst, (ref) => context.insertBefore(parent, ref, afterOLast));
      os.splice(oldLastIndex, 0, os.splice(oldFirstIndex, 1)[0]);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
    } else if (oLast.key === nFirst.key) {
      insertBefore(parent, oLast, oFirst, context);
      os.splice(oldFirstIndex, 0, os.splice(oldLastIndex, 1)[0]);
      diff(os[oldFirstIndex++], nFirst, parent, context);
      newFirstIndex++;
    } else {
      found = false;
      tmp = oldFirstIndex;
      while (tmp <= oldLastIndex) {
        if (os[tmp].key === nFirst.key) {
          found = true;
          node = os[tmp];
          break;
        }
        tmp++;
      }
      if (found) {
        os.splice(oldFirstIndex, 0, os.splice(tmp, 1)[0]);
        diff(os[oldFirstIndex++], nFirst, parent, context);
        insertBefore(parent, node, os[oldFirstIndex], context);
        newFirstIndex++;
      } else {
        createElement(parent, 2 /* INSERT_BEFORE */, getFirstDOMRef(oFirst), nFirst, context);
        os.splice(oldFirstIndex++, 0, nFirst);
        newFirstIndex++;
        oldLastIndex++;
      }
    }
  }
}
function swap(os, l, r) {
  const k = os[l];
  os[l] = os[r];
  os[r] = k;
}

// ts/miso/event.ts
function delegator(mount, events, getVTree, debug, context) {
  for (const event of events) {
    context.addEventListener(mount, event.name, function(e) {
      listener(e, mount, getVTree, debug, context);
    }, event.capture);
  }
}
function listener(e, mount, getVTree, debug, context) {
  getVTree(function(vtree) {
    if (Array.isArray(e)) {
      for (const key of e) {
        dispatch(key, vtree, mount, debug, context);
      }
    } else {
      dispatch(e, vtree, mount, debug, context);
    }
  });
}
function dispatch(ev, vtree, mount, debug, context) {
  var target = context.getTarget(ev);
  if (target) {
    let stack = buildTargetToElement(mount, target, context);
    delegateEvent(ev, vtree, stack, debug, context);
  }
}
function buildTargetToElement(element, target, context) {
  var stack = [];
  while (!context.isEqual(element, target)) {
    stack.unshift(target);
    if (target && context.parentNode(target)) {
      target = context.parentNode(target);
    } else {
      return stack;
    }
  }
  return stack;
}
function delegateEvent(event, obj, stack, debug, context) {
  if (!stack.length) {
    if (debug) {
      console.warn('Event "' + event.type + '" did not find an event handler to dispatch on', obj, event);
    }
    return;
  } else if (stack.length > 1) {
    if (obj.type === 2 /* VText */) {
      return;
    } else if (obj.type === 3 /* VFrag */) {
      for (const child of obj.children) {
        delegateEvent(event, child, stack, debug, context);
      }
      return;
    } else if (obj.type === 0 /* VComp */) {
      if (!obj.child) {
        if (debug) {
          console.error("VComp has no child property set during event delegation", obj);
          console.error("This means the Component has not been fully mounted, this should never happen");
          throw new Error("VComp has no .child property set during event delegation");
        }
        return;
      }
      return delegateEvent(event, obj.child, stack, debug, context);
    } else if (obj.type === 1 /* VNode */) {
      if (context.isEqual(obj.domRef, stack[0])) {
        const eventObj = obj.events.captures[event.type];
        if (eventObj) {
          const options = eventObj.options;
          if (options.preventDefault)
            event.preventDefault();
          if (!event["captureStopped"]) {
            eventObj.runEvent(event, obj.domRef);
          }
          if (options.stopPropagation) {
            event["captureStopped"] = true;
          }
        }
        stack.splice(0, 1);
        for (const child of obj.children) {
          if (containsDOMRef(child, stack[0], context)) {
            delegateEvent(event, child, stack, debug, context);
            return;
          }
        }
      }
      return;
    }
  } else {
    if (obj.type === 0 /* VComp */) {
      if (obj.child) {
        delegateEvent(event, obj.child, stack, debug, context);
      }
    } else if (obj.type === 3 /* VFrag */) {
      for (const child of obj.children) {
        delegateEvent(event, child, stack, debug, context);
      }
    } else if (obj.type === 1 /* VNode */) {
      const eventCaptureObj = obj.events.captures[event.type];
      if (eventCaptureObj && !event["captureStopped"]) {
        const options = eventCaptureObj.options;
        if (context.isEqual(stack[0], obj.domRef)) {
          if (options.preventDefault)
            event.preventDefault();
          eventCaptureObj.runEvent(event, stack[0]);
          if (options.stopPropagation)
            event["captureStopped"] = true;
        }
      }
      const eventObj = obj.events.bubbles[event.type];
      if (eventObj && !event["captureStopped"]) {
        const options = eventObj.options;
        if (context.isEqual(stack[0], obj.domRef)) {
          if (options.preventDefault)
            event.preventDefault();
          eventObj.runEvent(event, stack[0]);
          if (!options.stopPropagation) {
            propagateWhileAble(obj.parent, event);
          }
        }
      } else {
        if (!event["captureStopped"]) {
          propagateWhileAble(obj.parent, event);
        }
      }
    }
  }
}
function propagateWhileAble(vtree, event) {
  while (vtree) {
    switch (vtree.type) {
      case 2 /* VText */:
        break;
      case 3 /* VFrag */:
        vtree = vtree.parent;
        break;
      case 1 /* VNode */:
        const eventObj = vtree.events.bubbles[event.type];
        if (eventObj) {
          const options = eventObj.options;
          if (options.preventDefault)
            event.preventDefault();
          eventObj.runEvent(event, vtree.domRef);
          if (options.stopPropagation) {
            return;
          }
        }
        vtree = vtree.parent;
        break;
      case 0 /* VComp */:
        if (!vtree.eventPropagation)
          return;
        vtree = vtree.parent;
        break;
    }
  }
}
function eventJSON(at, obj) {
  if (typeof at[0] === "object") {
    var ret = [];
    for (var i = 0;i < at.length; i++) {
      ret.push(eventJSON(at[i], obj));
    }
    return ret;
  }
  for (const a of at)
    obj = obj[a];
  var newObj;
  if (obj instanceof Array || "length" in obj && obj["localName"] !== "select") {
    newObj = [];
    for (var j = 0;j < obj.length; j++) {
      newObj.push(eventJSON([], obj[j]));
    }
    return newObj;
  }
  newObj = {};
  for (var key in getAllPropertyNames(obj)) {
    if (obj["localName"] === "input" && (key === "selectionDirection" || key === "selectionStart" || key === "selectionEnd")) {
      continue;
    }
    if (typeof obj[key] == "string" || typeof obj[key] == "number" || typeof obj[key] == "boolean") {
      newObj[key] = obj[key];
    }
  }
  return newObj;
}
function containsDOMRef(vtree, target, context) {
  switch (vtree.type) {
    case 3 /* VFrag */:
      for (const child of vtree.children)
        if (containsDOMRef(child, target, context))
          return true;
      return false;
    case 0 /* VComp */:
      return vtree.child ? containsDOMRef(vtree.child, target, context) : false;
    default:
      return context.isEqual(vtree.domRef, target);
  }
}
function getAllPropertyNames(obj) {
  var props = {}, i = 0;
  do {
    var names = Object.getOwnPropertyNames(obj);
    for (i = 0;i < names.length; i++) {
      props[names[i]] = null;
    }
  } while (obj = Object.getPrototypeOf(obj));
  return props;
}

// ts/miso/context/dom.ts
var eventContext = {
  addEventListener: (mount, event, listener2, capture) => {
    mount.addEventListener(event, listener2, capture);
  },
  delegator: (mount, events, getVTree, debug, ctx) => {
    delegator(mount, events, getVTree, debug, ctx);
  },
  isEqual: (x, y) => {
    return x === y;
  },
  getTarget: (e) => {
    return e.target;
  },
  parentNode: (node) => {
    return node.parentNode;
  }
};
var hydrationContext = {
  getInlineStyle: (node, key) => {
    return node.style[key];
  },
  firstChild: (node) => {
    return node.firstChild;
  },
  lastChild: (node) => {
    return node.lastChild;
  },
  getAttribute: (node, key) => {
    if (key === "class")
      return node.className;
    if (key in node)
      return node[key];
    return node.getAttribute(key);
  },
  getTag: (node) => {
    return node.nodeName;
  },
  getTextContent: (node) => {
    return node.textContent;
  },
  children: (node) => {
    return node.childNodes;
  }
};
var componentContext = {
  mountComponent: function(componentId, model) {
    return;
  },
  unmountComponent: function(componentId) {
    return;
  },
  modelHydration: function(componentId, model) {
    return;
  }
};
var drawingContext = {
  nextSibling: (node) => {
    if (node.nextSibling) {
      switch (node.nextSibling.type) {
        case 0 /* VComp */:
        case 3 /* VFrag */:
          return getFirstDOMRef(node.nextSibling);
        default:
          return node.nextSibling.domRef;
      }
    }
    return null;
  },
  createTextNode: (s) => {
    return document.createTextNode(s);
  },
  createElementNS: (ns, tag) => {
    return document.createElementNS(ns, tag);
  },
  appendChild: (parent, child) => {
    return parent.appendChild(child);
  },
  replaceChild: (parent, n, old) => {
    return parent.replaceChild(n, old);
  },
  removeChild: (parent, child) => {
    return parent.removeChild(child);
  },
  createElement: (tag) => {
    return document.createElement(tag);
  },
  addClass: (className, domRef) => {
    if (className)
      domRef.classList.add(className);
  },
  removeClass: (className, domRef) => {
    if (className)
      domRef.classList.remove(className);
  },
  insertBefore: (parent, child, node) => {
    return parent.insertBefore(child, node);
  },
  swapDOMRefs: (oLast, oFirst, p) => {
    const tmp = oLast.nextSibling;
    p.insertBefore(oLast, oFirst);
    p.insertBefore(oFirst, tmp);
    return;
  },
  setInlineStyle: (cCss, nCss, node) => {
    var result;
    for (const key in cCss) {
      result = nCss[key];
      if (!result) {
        if (key in node.style) {
          node.style[key] = "";
        } else {
          node.style.setProperty(key, "");
        }
      } else if (result !== cCss[key]) {
        if (key in node.style) {
          node.style[key] = result;
        } else {
          node.style.setProperty(key, result);
        }
      }
    }
    for (const n in nCss) {
      if (cCss && cCss[n])
        continue;
      if (n in node.style) {
        node.style[n] = nCss[n];
      } else {
        node.style.setProperty(n, nCss[n]);
      }
    }
    return;
  },
  setAttribute: (node, key, value) => {
    return node.setAttribute(key, value);
  },
  setAttributeNS: (node, ns, key, value) => {
    return node.setAttributeNS(ns, key, value);
  },
  removeAttribute: (node, key) => {
    return node.removeAttribute(key);
  },
  setTextContent: (node, text) => {
    node.textContent = text;
    return;
  },
  flush: () => {
    return;
  },
  getHead: function() {
    return document.head;
  },
  getRoot: function() {
    return document.body;
  }
};

// ts/miso/hydrate.ts
function collapseSiblingTextNodes(vs) {
  var ax = 0, adjusted = vs.length > 0 ? [vs[0]] : [];
  for (var ix = 1;ix < vs.length; ix++) {
    if (adjusted[ax].type === 2 /* VText */ && vs[ix].type === 2 /* VText */) {
      adjusted[ax].text += vs[ix].text;
      continue;
    }
    adjusted[++ax] = vs[ix];
  }
  for (const v of adjusted) {
    if (v.type === 3 /* VFrag */) {
      v.children = collapseSiblingTextNodes(v.children);
    }
  }
  return adjusted;
}
function hydrate(logLevel, mountPoint, vtree, context, drawingContext2) {
  if (!vtree || !mountPoint)
    return false;
  if (mountPoint.nodeType === 3)
    return false;
  if (!walk(logLevel, vtree, context.firstChild(mountPoint), context, drawingContext2)) {
    if (logLevel) {
      console.warn("[DEBUG_HYDRATE] Could not copy DOM into virtual DOM, falling back to diff");
    }
    while (context.firstChild(mountPoint))
      drawingContext2.removeChild(mountPoint, context.lastChild(mountPoint));
    return false;
  } else {
    if (logLevel) {
      console.info("[DEBUG_HYDRATE] Successfully prerendered page");
    }
  }
  return true;
}
function diagnoseError(logLevel, vtree, node) {
  if (logLevel)
    console.warn("[DEBUG_HYDRATE] VTree differed from node", vtree, node);
}
function nextAfter(tree) {
  return getLastDOMRef(tree).nextSibling;
}
function walk(logLevel, vtree, node, context, drawingContext2) {
  switch (vtree.type) {
    case 0 /* VComp */:
      let mounted = vtree.mount(node.parentNode);
      vtree.componentId = mounted.componentId;
      vtree.child = mounted.componentTree;
      mounted.componentTree.parent = vtree;
      if (!walk(logLevel, vtree.child, node, context, drawingContext2)) {
        return false;
      }
      break;
    case 3 /* VFrag */:
      vtree.children = collapseSiblingTextNodes(vtree.children);
      for (const child of vtree.children) {
        if (!node) {
          diagnoseError(logLevel, child, null);
          return false;
        }
        if (!walk(logLevel, child, node, context, drawingContext2))
          return false;
        node = nextAfter(child);
      }
      break;
    case 2 /* VText */:
      if (node.nodeType !== 3 || vtree.text.trim() !== node.textContent.trim()) {
        diagnoseError(logLevel, vtree, node);
        return false;
      }
      vtree.domRef = node;
      break;
    case 1 /* VNode */:
      if (node.nodeType !== 1) {
        diagnoseError(logLevel, vtree, node);
        return false;
      }
      vtree.domRef = node;
      vtree.children = collapseSiblingTextNodes(vtree.children);
      callCreated(node, vtree, drawingContext2);
      let domCursor = node.firstChild;
      for (var i = 0;i < vtree.children.length; i++) {
        const vdomChild = vtree.children[i];
        if (!domCursor) {
          diagnoseError(logLevel, vdomChild, null);
          return false;
        }
        if (!walk(logLevel, vdomChild, domCursor, context, drawingContext2)) {
          return false;
        }
        domCursor = nextAfter(vdomChild);
      }
      break;
  }
  return true;
}

// ts/index.ts
globalThis["miso"] = {
  hydrationContext,
  eventContext,
  drawingContext,
  componentContext,
  diff,
  hydrate,
  version,
  callBlur,
  callFocus,
  callSelect,
  callSetSelectionRange,
  eventJSON,
  fetchCore,
  eventSourceConnect,
  eventSourceClose,
  websocketConnect,
  websocketClose,
  websocketSend,
  updateRef,
  inline,
  typeOf,
  mathRandom,
  getRandomValues,
  splitmix32,
  populateClass,
  delegateEvent,
  delegator: eventContext.delegator,
  setDrawingContext: function(name) {
    const drawing = globalThis[name]["drawingContext"];
    const events = globalThis[name]["eventContext"];
    const components = globalThis[name]["componentContext"];
    if (!drawing) {
      console.error('Custom rendering engine ("drawingContext") is not defined at globalThis[name].drawingContext', name);
    }
    if (!events) {
      console.error('Custom event delegation ("eventContext") is not defined at globalThis[name].eventContext', name);
    }
    if (!components) {
      console.error('Custom component context ("componentContext") is not defined at globalThis[name].componentContext', name);
    }
    globalThis["miso"]["drawingContext"] = drawing;
    globalThis["miso"]["eventContext"] = events;
    globalThis["miso"]["componentContext"] = components;
  }
};
