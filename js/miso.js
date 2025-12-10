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
    if (n.tag === c.tag && n.key === c.key) {
      n.domRef = c.domRef;
      diffAttrs(c, n, context);
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
  switch (c.type) {
    case 2 /* VText */:
      break;
    default:
      callBeforeDestroyedRecursive(c);
      break;
  }
  switch (n.type) {
    case 2 /* VText */:
      switch (c.type) {
        default:
          n.domRef = context.createTextNode(n.text);
          context.replaceChild(parent, n.domRef, c.domRef);
          break;
      }
      break;
    default:
      context.replaceChild(parent, createElement(n, context), c.domRef);
      break;
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
    default:
      callBeforeDestroyedRecursive(c);
      break;
  }
  context.removeChild(parent, c.domRef);
  switch (c.type) {
    case 2 /* VText */:
      break;
    default:
      callDestroyedRecursive(c);
      break;
  }
}
function callDestroyedRecursive(c) {
  callDestroyed(c);
  for (const child of c.children) {
    if (child.type === 1 /* VNode */ || child.type === 0 /* VComp */) {
      callDestroyedRecursive(child);
    }
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
      if (c.onBeforeUnmounted)
        c.onBeforeUnmounted();
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
  callBeforeDestroyed(c);
  for (const child of c.children)
    if (child.type === 1 /* VNode */ || child.type === 0 /* VComp */)
      callBeforeDestroyedRecursive(child);
}
function diffAttrs(c, n, context) {
  diffProps(c ? c.props : {}, n.props, n.domRef, n.ns === "svg", context);
  diffClass(c ? c.classList : null, n.classList, n.domRef, context);
  diffCss(c ? c.css : {}, n.css, n.domRef, context);
  if (n.type === 1 /* VNode */) {
    diffChildren(c ? c.children : [], n.children, n.domRef, context);
    drawCanvas(n);
  }
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
    if (cProps && cProps[n])
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
function diffChildren(cs, ns, parent, context) {
  if (shouldSync(cs, ns)) {
    syncChildren(cs, ns, parent, context);
  } else {
    for (let i = 0;i < Math.max(ns.length, cs.length); i++)
      diff(cs[i], ns[i], parent, context);
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
function callCreated(n, context) {
  switch (n.type) {
    case 0 /* VComp */:
      if (n.onBeforeMounted)
        n.onBeforeMounted();
      mountComponent(n, context);
      break;
    case 1 /* VNode */:
      if (n.onCreated)
        n.onCreated(n.domRef);
      break;
  }
  return n.domRef;
}
function createElement(n, context) {
  switch (n.type) {
    case 0 /* VComp */:
      if (n.onBeforeMounted)
        n.onBeforeMounted();
      populateDomRef(n, context);
      mountComponent(n, context);
      break;
    case 1 /* VNode */:
      if (n.onBeforeCreated)
        n.onBeforeCreated();
      populateDomRef(n, context);
      if (n.onCreated)
        n.onCreated(n.domRef);
      break;
  }
  diffAttrs(null, n, context);
  return n.domRef;
}
function drawCanvas(c) {
  if (c.tag === "canvas" && c.draw)
    c.draw(c.domRef);
}
function unmountComponent(c) {
  if (c.onUnmounted)
    c.onUnmounted(c.domRef);
  c.unmount(c.domRef);
}
function mountComponent(obj, context) {
  obj.mount(obj, (componentId, componentTree) => {
    obj.children.push(componentTree);
    context.appendChild(obj.domRef, componentTree.domRef);
    if (obj.onMounted)
      obj.onMounted(obj.domRef);
  });
}
function create(obj, parent, context) {
  if (obj.type === 2 /* VText */) {
    obj.domRef = context.createTextNode(obj.text);
    context.appendChild(parent, obj.domRef);
  } else {
    context.appendChild(parent, createElement(obj, context));
  }
}
function syncChildren(os, ns, parent, context) {
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
      diff(null, nFirst, parent, context);
      context.insertBefore(parent, nFirst.domRef, oFirst ? oFirst.domRef : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    } else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        context.removeChild(parent, os[oldLastIndex--].domRef);
      }
      os.splice(oldFirstIndex, tmp - oldFirstIndex + 1);
      break;
    } else if (oFirst.key === nFirst.key) {
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent, context);
    } else if (oLast.key === nLast.key) {
      diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
    } else if (oFirst.key === nLast.key && nFirst.key === oLast.key) {
      context.swapDOMRefs(oLast.domRef, oFirst.domRef, parent);
      swap(os, oldFirstIndex, oldLastIndex);
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent, context);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
    } else if (oFirst.key === nLast.key) {
      context.insertBefore(parent, oFirst.domRef, context.nextSibling(oLast));
      os.splice(oldLastIndex, 0, os.splice(oldFirstIndex, 1)[0]);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent, context);
    } else if (oLast.key === nFirst.key) {
      context.insertBefore(parent, oLast.domRef, oFirst.domRef);
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
        context.insertBefore(parent, node.domRef, os[oldFirstIndex].domRef);
        newFirstIndex++;
      } else {
        switch (nFirst.type) {
          case 2 /* VText */:
            nFirst.domRef = context.createTextNode(nFirst.text);
            context.insertBefore(parent, nFirst.domRef, oFirst.domRef);
            break;
          default:
            context.insertBefore(parent, createElement(nFirst, context), oFirst.domRef);
            break;
        }
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
function delegate(mount, events, getVTree, debug, context) {
  for (const event of events) {
    context.addEventListener(mount, event.name, function(e) {
      listener(e, mount, getVTree, debug, context);
    }, event.capture);
  }
}
function undelegate(mount, events, getVTree, debug, context) {
  for (const event of events) {
    context.removeEventListener(mount, event.name, function(e) {
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
    if (obj.type === 0 /* VComp */ || obj.type === 1 /* VNode */) {
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
      }
      for (const child of obj.children) {
        if (child.type === 0 /* VComp */ || child.type === 1 /* VNode */) {
          if (context.isEqual(child.domRef, stack[0])) {
            delegateEvent(event, child, stack, debug, context);
          }
        }
      }
    }
  } else {
    if (obj.type === 1 /* VNode */) {
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
  return adjusted;
}
function preamble(mountPoint, context) {
  var mountChildIdx = 0, node;
  var root = context.getRoot();
  if (!mountPoint) {
    if (root.childNodes.length > 0) {
      node = root.firstChild;
    } else {
      node = root.appendChild(context.createElement("div"));
    }
  } else if (mountPoint.childNodes.length === 0) {
    node = mountPoint.appendChild(context.createElement("div"));
  } else {
    while (mountPoint.childNodes[mountChildIdx] && (mountPoint.childNodes[mountChildIdx].nodeType === 3 || mountPoint.childNodes[mountChildIdx].localName === "script")) {
      mountChildIdx++;
    }
    if (!mountPoint.childNodes[mountChildIdx]) {
      node = root.appendChild(context.createElement("div"));
    } else {
      node = mountPoint.childNodes[mountChildIdx];
    }
  }
  return node;
}
function hydrate(logLevel, mountPoint, vtree, context, drawingContext) {
  const node = preamble(mountPoint, drawingContext);
  if (!walk(logLevel, vtree, node, context, drawingContext)) {
    if (logLevel) {
      console.warn("[DEBUG_HYDRATE] Could not copy DOM into virtual DOM, falling back to diff");
    }
    while (context.firstChild(node))
      drawingContext.removeChild(node, context.lastChild(node));
    vtree.domRef = node;
    switch (vtree.type) {
      case 2 /* VText */:
        break;
      default:
        diffAttrs(null, vtree, drawingContext);
        break;
    }
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
function parseColor(input) {
  if (input.substr(0, 1) == "#") {
    const collen = (input.length - 1) / 3;
    const fact = [17, 1, 0.062272][collen - 1];
    return [
      Math.round(parseInt(input.substr(1, collen), 16) * fact),
      Math.round(parseInt(input.substr(1 + collen, collen), 16) * fact),
      Math.round(parseInt(input.substr(1 + 2 * collen, collen), 16) * fact)
    ];
  } else
    return input.split("(")[1].split(")")[0].split(",").map((x) => {
      return +x;
    });
}
function integrityCheck(vtree, context, drawingContext) {
  return check(true, vtree, context, drawingContext);
}
function check(result, vtree, context, drawingContext) {
  if (vtree.type == 2 /* VText */) {
    if (context.getTag(vtree.domRef) !== "#text") {
      console.warn("VText domRef not a TEXT_NODE", vtree);
      result = false;
    } else if (vtree.text !== context.getTextContent(vtree.domRef)) {
      console.warn("VText node content differs", vtree);
      result = false;
    }
  } else {
    if (vtree.tag.toUpperCase() !== context.getTag(vtree.domRef).toUpperCase()) {
      console.warn("Integrity check failed, tags differ", vtree.tag.toUpperCase(), context.getTag(vtree.domRef));
      result = false;
    }
    if ("children" in vtree && vtree.children.length !== context.children(vtree.domRef).length) {
      console.warn("Integrity check failed, children lengths differ", vtree, vtree.children, context.children(vtree.domRef));
      result = false;
    }
    for (const key in vtree.props) {
      if (key === "href" || key === "src") {
        const absolute = window.location.origin + "/" + vtree.props[key], url = context.getAttribute(vtree.domRef, key), relative = vtree.props[key];
        if (absolute !== url && relative !== url && relative + "/" !== url && absolute + "/" !== url) {
          console.warn("Property " + key + " differs", vtree.props[key], context.getAttribute(vtree.domRef, key));
          result = false;
        }
      } else if (key === "height" || key === "width") {
        if (parseFloat(vtree.props[key]) !== parseFloat(context.getAttribute(vtree.domRef, key))) {
          console.warn("Property " + key + " differs", vtree.props[key], context.getAttribute(vtree.domRef, key));
          result = false;
        }
      } else if (key === "class" || key === "className") {
        if (vtree.props[key] !== context.getAttribute(vtree.domRef, "class")) {
          console.warn("Property class differs", vtree.props[key], context.getAttribute(vtree.domRef, "class"));
          result = false;
        }
      } else if (vtree.props[key] !== context.getAttribute(vtree.domRef, key)) {
        console.warn("Property " + key + " differs", vtree.props[key], context.getAttribute(vtree.domRef, key));
        result = false;
      }
    }
    for (const key in vtree.css) {
      if (key === "color") {
        if (parseColor(context.getInlineStyle(vtree.domRef, key)).toString() !== parseColor(vtree.css[key]).toString()) {
          console.warn("Style " + key + " differs", vtree.css[key], context.getInlineStyle(vtree.domRef, key));
          result = false;
        }
      } else if (vtree.css[key] !== context.getInlineStyle(vtree.domRef, key)) {
        console.warn("Style " + key + " differs", vtree.css[key], context.getInlineStyle(vtree.domRef, key));
        result = false;
      }
    }
    for (const child of vtree.children) {
      const value = check(result, child, context, drawingContext);
      result = result && value;
    }
  }
  return result;
}
function walk(logLevel, vtree, node, context, drawingContext) {
  switch (vtree.type) {
    case 0 /* VComp */:
      vtree.domRef = node;
      callCreated(vtree, drawingContext);
      break;
    case 2 /* VText */:
      vtree.domRef = node;
      break;
    case 1 /* VNode */:
      vtree.domRef = node;
      vtree.children = collapseSiblingTextNodes(vtree.children);
      callCreated(vtree, drawingContext);
      for (var i = 0;i < vtree.children.length; i++) {
        const vdomChild = vtree.children[i];
        const domChild = node.childNodes[i];
        if (!domChild) {
          diagnoseError(logLevel, vdomChild, domChild);
          return false;
        }
        switch (vdomChild.type) {
          case 2 /* VText */:
            if (domChild.nodeType !== 3) {
              diagnoseError(logLevel, vdomChild, domChild);
              return false;
            }
            if (vdomChild.text === domChild.textContent) {
              vdomChild.domRef = context.children(node)[i];
            } else {
              diagnoseError(logLevel, vdomChild, domChild);
              return false;
            }
            break;
          default:
            if (domChild.nodeType !== 1)
              return false;
            vdomChild.domRef = domChild;
            walk(logLevel, vdomChild, domChild, context, drawingContext);
            break;
        }
      }
  }
  return true;
}

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
function getParentComponentId(vcompNode) {
  var climb = function(node) {
    let parentComponentId = null;
    while (node && node.parentNode) {
      if ("componentId" in node.parentNode) {
        parentComponentId = node.parentNode["componentId"];
        break;
      }
      node = node.parentNode;
    }
    return parentComponentId;
  };
  return climb(vcompNode);
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

// ts/miso/context/dom.ts
var eventContext = {
  addEventListener: (mount, event, listener2, capture) => {
    mount.addEventListener(event, listener2, capture);
  },
  removeEventListener: (mount, event, listener2, capture) => {
    mount.removeEventListener(event, listener2, capture);
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
  mountComponent: function(events, componentId, model) {
    return;
  },
  unmountComponent: function(componentId) {
    return;
  },
  modelHydration: function(model) {
    return;
  }
};
var drawingContext = {
  nextSibling: (node) => {
    return node.domRef.nextSibling;
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
  swapDOMRefs: (a, b, p) => {
    const tmp = a.nextSibling;
    p.insertBefore(a, b);
    p.insertBefore(b, tmp);
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
  getRoot: function() {
    return document.body;
  }
};

// ts/index.ts
globalThis["miso"] = {
  hydrationContext,
  eventContext,
  drawingContext,
  componentContext,
  diff,
  hydrate,
  version,
  delegate,
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
  undelegate,
  populateClass,
  getParentComponentId,
  integrityCheck,
  setDrawingContext: function(name) {
    const drawing = globalThis[name]["drawingContext"];
    const events = globalThis[name]["eventContext"];
    if (!drawing) {
      console.error('Custom rendering engine ("drawingContext") is not defined at globalThis[name].drawingContext', name);
    }
    if (!events) {
      console.error('Custom event delegation ("eventContext") is not defined at globalThis[name].eventContext', name);
    }
    globalThis["miso"]["drawingContext"] = drawing;
    globalThis["miso"]["eventContext"] = events;
  }
};
