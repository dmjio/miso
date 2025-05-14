// ts/miso/util.ts
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
function setComponent(node, componentId) {
  node.setAttribute("data-component-id", componentId);
}
function fetchJSON(url, method, body, headers, successful, errorful) {
  var options = { method, headers };
  if (body) {
    options["body"] = body;
  }
  fetch(url, options).then((response) => {
    if (!response.ok) {
      throw new Error(response.statusText);
    }
    return response.json();
  }).then(successful).catch(errorful);
}
function shouldSync(node) {
  if (node.children.length === 0) {
    return false;
  }
  var enterSync = true;
  for (const child of node.children) {
    if (!child.key) {
      enterSync = false;
      break;
    }
  }
  return enterSync;
}
var version = "1.9.0.0";

// ts/miso/smart.ts
function vnode(props) {
  var node = union(mkVNode(), props);
  if (!node["shouldSync"])
    node["shouldSync"] = shouldSync(node);
  return node;
}
function union(obj, updates) {
  return Object.assign({}, obj, updates);
}
function mkVNode() {
  return {
    props: {},
    css: {},
    children: [],
    ns: "html",
    domRef: null,
    tag: "div",
    key: null,
    events: {},
    onDestroyed: () => {},
    onBeforeDestroyed: () => {},
    onCreated: () => {},
    onBeforeCreated: () => {},
    shouldSync: false,
    type: "vnode"
  };
}

// ts/miso/dom.ts
function diff(currentObj, newObj, parent) {
  if (!currentObj && !newObj)
    return;
  else if (!currentObj && newObj)
    create(newObj, parent);
  else if (!newObj)
    destroy(currentObj, parent);
  else {
    if (currentObj["type"] === newObj["type"]) {
      diffNodes(currentObj, newObj, parent);
    } else {
      replace(currentObj, newObj, parent);
    }
  }
}
function replace(c, n, parent) {
  callBeforeDestroyedRecursive(c);
  if (n["type"] === "vtext") {
    n["domRef"] = document.createTextNode(n["text"]);
    parent.replaceChild(n["domRef"], c["domRef"]);
  } else {
    createElement(n, (ref) => {
      parent.replaceChild(ref, c["domRef"]);
    });
  }
  callDestroyedRecursive(c);
}
function destroy(obj, parent) {
  callBeforeDestroyedRecursive(obj);
  parent.removeChild(obj["domRef"]);
  callDestroyedRecursive(obj);
}
function diffNodes(c, n, parent) {
  if (c["type"] === "vtext") {
    if (c["text"] !== n["text"])
      c["domRef"].textContent = n["text"];
    n["domRef"] = c["domRef"];
    return;
  }
  var componentIdCheck = function(n2, c2) {
    if (n2["type"] === "vcomp" && !n2["data-component-id"].startsWith("miso-component-id")) {
      return n2["data-component-id"] === c2["data-component-id"];
    }
    return true;
  };
  if (c["tag"] === n["tag"] && n["key"] === c["key"] && n["type"] === c["type"] && componentIdCheck(n, c)) {
    n["domRef"] = c["domRef"];
    populate(c, n);
  } else {
    replace(c, n, parent);
  }
}
function callDestroyedRecursive(obj) {
  callDestroyed(obj);
  for (const i in obj["children"]) {
    callDestroyedRecursive(obj["children"][i]);
  }
}
function callDestroyed(obj) {
  if (obj["onDestroyed"])
    obj["onDestroyed"]();
  if (obj["type"] === "vcomp")
    unmountComponent(obj);
}
function callBeforeDestroyed(obj) {
  if (obj["onBeforeDestroyed"])
    obj["onBeforeDestroyed"]();
}
function callBeforeDestroyedRecursive(obj) {
  if (obj["type"] === "vcomp" && obj["onBeforeUnmounted"]) {
    obj["onBeforeUnmounted"]();
  }
  callBeforeDestroyed(obj);
  for (const i in obj["children"]) {
    callBeforeDestroyedRecursive(obj["children"][i]);
  }
}
function callCreated(obj) {
  if (obj["onCreated"])
    obj["onCreated"]();
  if (obj["type"] === "vcomp")
    mountComponent(obj);
}
function callBeforeCreated(obj) {
  if (obj["onBeforeCreated"])
    obj["onBeforeCreated"]();
}
function populate(c, n) {
  if (n["type"] !== "vtext") {
    if (!c)
      c = vnode({});
    diffProps(c["props"], n["props"], n["domRef"], n["ns"] === "svg");
    diffCss(c["css"], n["css"], n["domRef"]);
    if (n["type"] === "vnode") {
      diffChildren(c, n, n["domRef"]);
    }
    drawCanvas(n);
  }
}
function diffProps(cProps, nProps, node, isSvg) {
  var newProp;
  for (const c in cProps) {
    newProp = nProps[c];
    if (newProp === undefined) {
      if (isSvg || !(c in node)) {
        node.removeAttribute(cProps[c]);
      } else {
        node[c] = "";
      }
    } else {
      if (newProp === cProps[c] && c !== "checked" && c !== "value")
        continue;
      if (isSvg) {
        if (c === "href") {
          node.setAttributeNS("http://www.w3.org/1999/xlink", "href", newProp);
        } else {
          node.setAttribute(c, newProp);
        }
      } else if (c in node && !(c === "list" || c === "form")) {
        node[c] = newProp;
      } else {
        node.setAttribute(c, newProp);
      }
    }
  }
  for (const n in nProps) {
    if (cProps && cProps[n])
      continue;
    newProp = nProps[n];
    if (isSvg) {
      if (n === "href") {
        node.setAttributeNS("http://www.w3.org/1999/xlink", "href", newProp);
      } else {
        node.setAttribute(n, newProp);
      }
    } else if (n in node && !(n === "list" || n === "form")) {
      node[n] = nProps[n];
    } else {
      node.setAttribute(n, newProp);
    }
  }
}
function diffCss(cCss, nCss, node) {
  var result;
  for (const c in cCss) {
    result = nCss[c];
    if (!result) {
      node.style[c] = "";
    } else if (result !== cCss[c]) {
      node.style[c] = result;
    }
  }
  for (const n in nCss) {
    if (cCss && cCss[n])
      continue;
    node.style[n] = nCss[n];
  }
}
function diffChildren(c, n, parent) {
  if (c.shouldSync && n.shouldSync) {
    syncChildren(c.children, n.children, parent);
  } else {
    const longest = n.children.length > c.children.length ? n.children.length : c.children.length;
    for (let i = 0;i < longest; i++)
      diff(c.children[i], n.children[i], parent);
  }
}
function populateDomRef(obj) {
  if (obj["ns"] === "svg") {
    obj["domRef"] = document.createElementNS("http://www.w3.org/2000/svg", obj["tag"]);
  } else if (obj["ns"] === "mathml") {
    obj["domRef"] = document.createElementNS("http://www.w3.org/1998/Math/MathML", obj["tag"]);
  } else {
    obj["domRef"] = document.createElement(obj["tag"]);
  }
}
function createElement(obj, cb) {
  callBeforeCreated(obj);
  populateDomRef(obj);
  cb(obj["domRef"]);
  populate(null, obj);
  callCreated(obj);
}
function drawCanvas(obj) {
  if (obj["tag"] === "canvas" && "draw" in obj) {
    obj["draw"](obj["domRef"]);
  }
}
function unmountComponent(obj) {
  if ("onUnmounted" in obj)
    obj["onUnmounted"](obj["data-component-id"]);
  obj["unmount"]();
}
function mountComponent(obj) {
  const componentId = obj["data-component-id"], nodeList = document.querySelectorAll("[data-component-id='" + componentId + "']");
  if (nodeList.length > 0) {
    console.error('AlreadyMountedException: Component "' + componentId + "' is already mounted");
    return;
  }
  obj["domRef"].setAttribute("data-component-id", componentId);
  if (obj["onBeforeMounted"])
    obj["onBeforeMounted"]();
  obj["mount"]((component) => {
    obj["children"].push(component);
    obj["domRef"].appendChild(component["domRef"]);
    if (obj["onMounted"])
      obj["onMounted"](componentId);
  });
}
function create(obj, parent) {
  if (obj["type"] === "vtext") {
    obj["domRef"] = document.createTextNode(obj["text"]);
    parent.appendChild(obj["domRef"]);
  } else {
    createElement(obj, (e) => {
      parent.appendChild(e);
    });
  }
}
function syncChildren(os, ns, parent) {
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
      diff(null, nFirst, parent);
      parent.insertBefore(nFirst["domRef"], oFirst ? oFirst["domRef"] : null);
      os.splice(newFirstIndex, 0, nFirst);
      newFirstIndex++;
    } else if (newFirstIndex > newLastIndex) {
      tmp = oldLastIndex;
      while (oldLastIndex >= oldFirstIndex) {
        parent.removeChild(os[oldLastIndex--]["domRef"]);
      }
      os.splice(oldFirstIndex, tmp - oldFirstIndex + 1);
      break;
    } else if (oFirst["key"] === nFirst["key"]) {
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent);
    } else if (oLast["key"] === nLast["key"]) {
      diff(os[oldLastIndex--], ns[newLastIndex--], parent);
    } else if (oFirst["key"] === nLast["key"] && nFirst["key"] === oLast["key"]) {
      swapDOMRefs(oLast["domRef"], oFirst["domRef"], parent);
      swap(os, oldFirstIndex, oldLastIndex);
      diff(os[oldFirstIndex++], ns[newFirstIndex++], parent);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent);
    } else if (oFirst["key"] === nLast["key"]) {
      parent.insertBefore(oFirst["domRef"], oLast["domRef"].nextSibling);
      os.splice(oldLastIndex, 0, os.splice(oldFirstIndex, 1)[0]);
      diff(os[oldLastIndex--], ns[newLastIndex--], parent);
    } else if (oLast["key"] === nFirst["key"]) {
      parent.insertBefore(oLast["domRef"], oFirst["domRef"]);
      os.splice(oldFirstIndex, 0, os.splice(oldLastIndex, 1)[0]);
      diff(os[oldFirstIndex++], nFirst, parent);
      newFirstIndex++;
    } else {
      found = false;
      tmp = oldFirstIndex;
      while (tmp <= oldLastIndex) {
        if (os[tmp]["key"] === nFirst["key"]) {
          found = true;
          node = os[tmp];
          break;
        }
        tmp++;
      }
      if (found) {
        os.splice(oldFirstIndex, 0, os.splice(tmp, 1)[0]);
        diff(os[oldFirstIndex++], nFirst, parent);
        parent.insertBefore(node["domRef"], os[oldFirstIndex]["domRef"]);
        newFirstIndex++;
      } else {
        createElement(nFirst, (e) => {
          parent.insertBefore(e, oFirst["domRef"]);
        });
        os.splice(oldFirstIndex++, 0, nFirst);
        newFirstIndex++;
        oldLastIndex++;
      }
    }
  }
}
function swapDOMRefs(a, b, p) {
  const tmp = a.nextSibling;
  p.insertBefore(a, b);
  p.insertBefore(b, tmp);
}
function swap(os, l, r) {
  const k = os[l];
  os[l] = os[r];
  os[r] = k;
}

// ts/miso/event.ts
function delegate(mount, events, getVTree, debug) {
  for (const event of events) {
    mount.addEventListener(event["name"], function(e) {
      listener(e, mount, getVTree, debug);
      e.stopPropagation();
    }, event["capture"]);
  }
}
function undelegate(mount, events, getVTree, debug) {
  for (const event of events) {
    mount.removeEventListener(event["name"], function(e) {
      listener(e, mount, getVTree, debug);
    }, event["capture"]);
  }
}
function listener(e, mount, getVTree, debug) {
  getVTree(function(obj) {
    if (e.target) {
      delegateEvent(e, obj, buildTargetToElement(mount, e.target), [], debug);
    }
  });
}
function buildTargetToElement(element, target) {
  var stack = [];
  while (element !== target) {
    stack.unshift(target);
    target = target.parentNode;
  }
  return stack;
}
function delegateEvent(event, obj, stack, parentStack, debug) {
  if (!stack.length) {
    if (debug) {
      console.warn('Event "' + event.type + '" did not find an event handler to dispatch on', obj, event);
    }
    return;
  } else if (stack.length > 1) {
    parentStack.unshift(obj);
    for (const child of obj["children"]) {
      if (child["type"] === "vcomp")
        continue;
      if (child["domRef"] === stack[1]) {
        delegateEvent(event, child, stack.slice(1), parentStack, debug);
        break;
      }
    }
  } else {
    const eventObj = obj["events"][event.type];
    if (eventObj) {
      const options = eventObj["options"];
      if (options["preventDefault"]) {
        event.preventDefault();
      }
      eventObj["runEvent"](event);
      if (!options["stopPropagation"]) {
        propagateWhileAble(parentStack, event);
      }
    } else {
      propagateWhileAble(parentStack, event);
    }
  }
}
function propagateWhileAble(parentStack, event) {
  for (const vtree of parentStack) {
    if (vtree["events"][event.type]) {
      const eventObj = vtree["events"][event.type], options = eventObj["options"];
      if (options["preventDefault"])
        event.preventDefault();
      eventObj["runEvent"](event);
      if (options["stopPropagation"]) {
        event.stopPropagation();
        break;
      }
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
    if (adjusted[ax]["type"] === "vtext" && vs[ix]["type"] === "vtext") {
      adjusted[ax]["text"] += vs[ix]["text"];
      continue;
    }
    adjusted[++ax] = vs[ix];
  }
  return adjusted;
}
function preamble(mountPoint) {
  var mountChildIdx = 0, node;
  if (!mountPoint) {
    if (document.body.childNodes.length > 0) {
      node = document.body.firstChild;
    } else {
      node = document.body.appendChild(document.createElement("div"));
    }
  } else if (mountPoint.childNodes.length === 0) {
    node = mountPoint.appendChild(document.createElement("div"));
  } else {
    while (mountPoint.childNodes[mountChildIdx] && (mountPoint.childNodes[mountChildIdx].nodeType === 3 || mountPoint.childNodes[mountChildIdx].localName === "script")) {
      mountChildIdx++;
    }
    if (!mountPoint.childNodes[mountChildIdx]) {
      node = document.body.appendChild(document.createElement("div"));
    } else {
      node = mountPoint.childNodes[mountChildIdx];
    }
  }
  return node;
}
function hydrate(logLevel, mountPoint, vtree) {
  const node = preamble(mountPoint);
  if (!walk(logLevel, vtree, node)) {
    if (logLevel) {
      console.warn("Could not copy DOM into virtual DOM, falling back to diff");
    }
    while (node.firstChild)
      node.removeChild(node.lastChild);
    vtree["domRef"] = node;
    populate(null, vtree);
    return false;
  } else {
    if (logLevel) {
      if (!integrityCheck(vtree)) {
        console.warn("Integrity check completed with errors");
      } else {
        console.info("Successfully prerendered page");
      }
    }
  }
  return true;
}
function diagnoseError(logLevel, vtree, node) {
  if (logLevel)
    console.warn("VTree differed from node", vtree, node);
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
function integrityCheck(vtree) {
  return check(true, vtree);
}
function check(result, vtree) {
  if (vtree["type"] == "vtext") {
    if (vtree["domRef"].nodeType !== 3) {
      console.warn("VText domRef not a TEXT_NODE", vtree);
      result = false;
    } else if (vtree["text"] !== vtree["domRef"].textContent) {
      console.warn("VText node content differs", vtree);
      result = false;
    }
  } else {
    if (vtree["tag"].toUpperCase() !== vtree["domRef"].tagName) {
      console.warn("Integrity check failed, tags differ", vtree["tag"].toUpperCase(), vtree["domRef"].tagName);
      result = false;
    }
    if ("children" in vtree && vtree["children"].length !== vtree["domRef"].childNodes.length) {
      console.warn("Integrity check failed, children lengths differ", vtree, vtree.children, vtree["domRef"].childNodes);
      result = false;
    }
    for (const key in vtree["props"]) {
      if (key === "href") {
        const absolute = window.location.origin + "/" + vtree["props"][key], url = vtree["domRef"][key], relative = vtree["props"][key];
        if (absolute !== url && relative !== url && relative + "/" !== url && absolute + "/" !== url) {
          console.warn("Property " + key + " differs", vtree["props"][key], vtree["domRef"][key]);
          result = false;
        }
      } else if (key === "height" || key === "width") {
        if (parseFloat(vtree["props"][key]) !== parseFloat(vtree["domRef"][key])) {
          console.warn("Property " + key + " differs", vtree["props"][key], vtree["domRef"][key]);
          result = false;
        }
      } else if (key === "class" || key === "className") {
        if (vtree["props"][key] !== vtree["domRef"].className) {
          console.warn("Property class differs", vtree["props"][key], vtree["domRef"].className);
          result = false;
        }
      } else if (!vtree["domRef"][key]) {
        if (vtree["props"][key] !== vtree["domRef"].getAttribute(key)) {
          console.warn("Property " + key + " differs", vtree["props"][key], vtree["domRef"].getAttribute(key));
          result = false;
        }
      } else if (vtree["props"][key] !== vtree["domRef"][key]) {
        console.warn("Property " + key + " differs", vtree["props"][key], vtree["domRef"][key]);
        result = false;
      }
    }
    for (const key in vtree["css"]) {
      if (key === "color") {
        if (parseColor(vtree["domRef"].style[key]).toString() !== parseColor(vtree["css"][key]).toString()) {
          console.warn("Style " + key + " differs", vtree["css"][key], vtree["domRef"].style[key]);
          result = false;
        }
      } else if (vtree["css"][key] !== vtree["domRef"].style[key]) {
        console.warn("Style " + key + " differs", vtree["css"][key], vtree["domRef"].style[key]);
        result = false;
      }
    }
    for (const child of vtree["children"]) {
      const value = check(result, child);
      result = result && value;
    }
  }
  return result;
}
function walk(logLevel, vtree, node) {
  switch (vtree["type"]) {
    case "vtext":
      vtree["domRef"] = node;
      break;
    default:
      vtree["domRef"] = node;
      vtree["children"] = collapseSiblingTextNodes(vtree["children"]);
      callCreated(vtree);
      for (var i = 0;i < vtree["children"].length; i++) {
        const vdomChild = vtree["children"][i];
        const domChild = node.childNodes[i];
        if (!domChild) {
          diagnoseError(logLevel, vdomChild, domChild);
          return false;
        }
        switch (vdomChild["type"]) {
          case "vtext":
            if (domChild.nodeType !== 3) {
              diagnoseError(logLevel, vdomChild, domChild);
              return false;
            }
            if (vdomChild["text"] === domChild.textContent) {
              vdomChild["domRef"] = node.childNodes[i];
            } else {
              diagnoseError(logLevel, vdomChild, domChild);
              return false;
            }
            break;
          case "vcomp":
            vdomChild["mount"]((component) => {
              vdomChild["children"].push(component);
              walk(logLevel, vdomChild, node.childNodes[i]);
            });
            break;
          default:
            if (domChild.nodeType !== 1)
              return false;
            vdomChild["domRef"] = node.childNodes[i];
            walk(logLevel, vdomChild, vdomChild["domRef"]);
        }
      }
  }
  return true;
}

// ts/index.ts
globalThis["miso"] = {};
globalThis["miso"]["diff"] = diff;
globalThis["miso"]["hydrate"] = hydrate;
globalThis["miso"]["version"] = version;
globalThis["miso"]["delegate"] = delegate;
globalThis["miso"]["callBlur"] = callBlur;
globalThis["miso"]["callFocus"] = callFocus;
globalThis["miso"]["eventJSON"] = eventJSON;
globalThis["miso"]["fetchJSON"] = fetchJSON;
globalThis["miso"]["undelegate"] = undelegate;
globalThis["miso"]["shouldSync"] = shouldSync;
globalThis["miso"]["integrityCheck"] = integrityCheck;
globalThis["miso"]["setComponent"] = setComponent;
