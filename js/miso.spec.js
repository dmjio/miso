/**
 * @jest-environment jsdom
 */

/* custom js imports */
const miso = require("./miso");

/* smart constructors */
function vcomp(mount, unmount, props, css, children, ref, oc, od, bd, key) {
  return {
    type: "vcomp",
    tag: "div",
    children: [],
    props: props,
    "data-component-id": "vcomp-id",
    css: css,
    ns: "HTML",
    domRef: ref,
    onCreated: oc,
    onDestroyed: od,
    onBeforeDestroyed: bd,
    key: key,
    mount: mount,
    unmount: unmount,
  };
}

function vnode(tag, children, props, css, ns, ref, oc, od, bd, key) {
  return {
    type: "vnode",
    tag: tag,
    children: children,
    props: props,
    css: css,
    ns: ns,
    domRef: ref,
    onCreated: oc,
    onDestroyed: od,
    onBeforeDestroyed: bd,
    key: key,
  };
}

function vnodeKeyed(tag, key) {
  return {
    type: "vnode",
    tag: tag,
    children: [vtext(key)],
    props: {},
    css: {},
    ns: "HTML",
    domRef: null,
    onCreated: null,
    onDestroyed: null,
    onBeforeDestroyed: null,
    key: key,
  };
}

function vnodeKids(tag, kids) {
  return {
    type: "vnode",
    tag: tag,
    children: kids,
    props: {},
    css: {},
    ns: "HTML",
    domRef: null,
    onCreated: null,
    onDestroyed: null,
    onBeforeDestroyed: null,
  };
}

function vtext(txt) {
  return {
    type: "vtext",
    text: txt,
  };
}

function vtextKeyed(txt, key) {
  return {
    type: "vtext",
    text: txt,
    key: key,
  };
}

describe("miso.js tests", () => {
  /* Reset DOM before each test */
  beforeEach(() => {
    document.body.innerHTML = "";
  });

  test("Should be null when miso.diffing two null virtual DOMs", () => {
    var document = window.document;
    const body = document.body;
    var c = null;
    var n = null;
    miso.diff(c, n, body);
    expect(body.childNodes.length).toBe(0);
  });

  test("Should create a new text node", () => {
    const body = document.body;
    var newNode = {
      type: "vtext",
      text: "foo",
    };
    miso.diff(null, newNode, body);
    expect(newNode.domRef.wholeText).toBe("foo");
  });

  test("Should window miso.diff two identical text nodes", () => {
    const body = document.body;
    var currentNode = {
      type: "vtext",
      text: "foo",
    };
    miso.diff(null, currentNode, body);
    expect(currentNode.domRef.wholeText).toBe("foo");
    var newNode = {
      type: "vtext",
      text: "foo",
    };
    miso.diff(currentNode, newNode, body);
    expect("foo").toBe(newNode.domRef.wholeText);
  });

  test("Should window miso.diff two window miso.different text nodes", () => {
    const body = document.body;
    var currentNode = {
      type: "vtext",
      text: "foo",
    };
    miso.diff(null, currentNode, body);
    expect(currentNode.domRef.wholeText).toBe("foo");
    var newNode = {
      type: "vtext",
      text: "bar",
    };
    miso.diff(currentNode, newNode, body);
    expect(newNode.domRef.wholeText).toBe("bar");
  });

  test("Should create a new DOM node", () => {
    var body = document.body;
    var currentNode = null;
    var newNode = vnode("div", []);
    miso.diff(currentNode, newNode, body);
    expect(body.children[0]).toBe(newNode.domRef);
  });

  test("Should detect duplicate component mounting", () => {
    var body = document.body;
    var mountCount = 0;
    var newComp1 = vcomp(
      () => {
        return mountCount++;
      },
      null,
      { "data-component-id": "vcomp-foo" },
      { "background-color": "red" },
      [],
    );
    miso.diff(null, newComp1, body);
    var newComp2 = vcomp(
      () => {
        return mountCount++;
      },
      null,
      { "data-component-id": "vcomp-foo" },
      { "background-color": "red" },
      [],
    );
    var newNode = vnode("div", [newComp2], {}, {}, "svg");
    miso.diff(null, newNode, body);
    expect(mountCount).toBe(1);
  });

  test("Should mount and unmount a component", () => {
    var body = document.body;
    var mountCount = 0;
    var unmountCount = 0;
    var mountFunc = function (cb) {
      mountCount++;
      var node = vnode("div", []);
      miso.diff(null, node, body);
      cb(node);
    };
    var newNode = vcomp(
      mountFunc,
      () => {
        return unmountCount++;
      },
      { id: "vcomp-foo" },
      {
        "background-color": "red",
      },
      [],
    );
    miso.diff(null, newNode, body);
    expect(mountCount).toBe(1);
    expect(newNode.children.length).toBe(1);
    expect(newNode.domRef.children.length).toBe(1);
    expect(newNode.domRef.id).toBe("vcomp-foo");
    expect(newNode.domRef.style["background-color"]).toBe("red");
    miso.diff(newNode, null, body);
    expect(unmountCount).toBe(1);
  });

  test("Should create an SVG DOM node", () => {
    var body = document.body;
    var currentNode = null;
    var newNode = vnode("div", [], {}, {}, "svg");
    miso.diff(currentNode, newNode, body);
    expect(body.children[0]).toBe(newNode.domRef);
  });

  test("Should create a MathML DOM node", () => {
    var body = document.body;
    var currentNode = null;
    var newNode = vnode("math", [], {}, {}, "mathml");
    miso.diff(currentNode, newNode, body);
    expect(body.children[0]).toBe(newNode.domRef);
  });

  test("Should create an SVG DOM node, with href attribute", () => {
    var body = document.body;
    var currentNode = null;
    var newNode = vnode(
      "ellipse",
      [],
      {
        href: "https://google.com",
      },
      {},
      "svg",
    );
    miso.diff(currentNode, newNode, body);
    expect(
      body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", "href"),
    ).toBe("https://google.com");
  });

  test("Should create an SVG DOM node, with href attribute, and change it", () => {
    var body = document.body;
    var currentNode = null;
    var newNode = vnode(
      "ellipse",
      [],
      {
        href: "https://google.com",
      },
      {},
      "svg",
    );
    miso.diff(currentNode, newNode, body);
    expect(
      body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", "href"),
    ).toBe("https://google.com");
    var newerNode = vnode(
      "ellipse",
      [],
      {
        href: "https://yahoo.com",
      },
      {},
      "svg",
    );
    miso.diff(newNode, newerNode, body);
    expect(
      body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", "href"),
    ).toBe("https://yahoo.com");
  });

  test("Should create an SVG DOM node, with regular attribute", () => {
    var body = document.body;
    var currentNode = null;
    var newNode = vnode(
      "ellipse",
      [],
      {
        rx: "100",
      },
      {},
      "svg",
    );
    miso.diff(currentNode, newNode, body);
    expect(body.children[0].getAttribute("rx")).toBe("100");
  });

  test("Should create an SVG DOM node, with regular attribute, and change it", () => {
    var body = document.body;
    var currentNode = null;
    var newNode = vnode(
      "ellipse",
      [],
      {
        rx: "100",
      },
      {},
      "svg",
    );
    miso.diff(currentNode, newNode, body);
    expect(body.children[0].getAttribute("rx")).toBe("100");
    var newerNode = vnode(
      "ellipse",
      [],
      {
        rx: "200",
      },
      {},
      "svg",
    );
    miso.diff(newNode, newerNode, body);
    expect(body.children[0].getAttribute("rx")).toBe("200");
  });

  test("Should replace a Node with a new Node of a window miso.different tag", () => {
    var body = document.body;

    // populate DOM
    var node = vnode("div", []);
    miso.diff(null, node, body);

    // Test node was populated
    expect(body.children.length).toBe(1);

    // Replace node
    var newNode = vnode("a", []);
    miso.diff(node, newNode, body);

    // Test node is removed from DOM
    expect(body.children[0].tagName).toBe("A");
  });

  test("Should create children", () => {
    var body = document.body;

    // populate DOM
    var node = vnode("div", [vnode("div", [])]);
    miso.diff(null, node, body);
    expect(node.domRef.children.length).toBe(1);
  });

  test("Should remove a child", () => {
    var body = document.body;

    // populate DOM
    var node = vnode("div", [vnode("div", [])]);
    miso.diff(null, node, body);
    expect(node.domRef.children.length).toBe(1);

    // populate DOM
    var newNode = vnode("div", []);
    miso.diff(node, newNode, body);
    expect(node.domRef.children.length).toBe(0);
  });

  test("Should Miso.Diff attrs of two Components", () => {
    var body = document.body;

    // populate DOM
    var mountCount = 0;
    var compNode1 = vcomp(
      () => {
        return mountCount++;
      },
      null,
      {
        "data-component-id": "vcomp-foo",
      },
      { "background-color": "red" },
    );

    miso.diff(null, compNode1, body);
    expect(mountCount).toBe(1);

    // Test node was populated
    expect(body.childNodes.length).toBe(1);
    expect(body.childNodes[0].style["background-color"]).toBe("red");

    // Replace node
    mountCount = 0;
    var compNode2 = vcomp(
      () => {
        return mountCount++;
      },
      null,
      {
        "data-component-id": "vcomp-foo",
      },
      { "background-color": "green" },
    );

    miso.diff(compNode1, compNode2, body);
    expect(body.childNodes[0].style["background-color"]).toBe("green");
  });

  test("Should replace Node with Component", () => {
    var body = document.body;

    // populate DOM
    var node = vnode("div", []);
    miso.diff(null, node, body);

    // Test node was populated
    expect(body.childNodes.length).toBe(1);

    // Replace node
    var mountCount = 0;
    var compNode = vcomp(() => {
      return mountCount++;
    });
    miso.diff(node, compNode, body);

    // Node is removed from DOM, Component is on the DOM
    expect(body.childNodes[0].getAttribute("data-component-id")).toBe(
      "vcomp-id",
    );
    expect(mountCount).toBe(1);
  });

  test("Should replace Text with Component", () => {
    var body = document.body;

    // populate DOM
    var node = vtext("foo");
    miso.diff(null, node, body);

    // Test node was populated
    expect(node.domRef.wholeText).toBe("foo");
    expect(body.childNodes.length).toBe(1);

    // Replace node
    var mountCount = 0;
    var compNode = vcomp(() => {
      return mountCount++;
    });
    miso.diff(node, compNode, body);

    // Node is removed from DOM, Component is on the DOM
    expect(body.childNodes[0].getAttribute("data-component-id")).toBe(
      "vcomp-id",
    );
    expect(mountCount).toBe(1);
  });

  test("Should replace Node with TextNode", () => {
    var body = document.body;

    // populate DOM
    var node = vnode("div", []);
    miso.diff(null, node, body);

    // Test node was populated
    expect(body.childNodes.length).toBe(1);

    // Replace node
    var textNode = vtext("fooo");
    miso.diff(node, textNode, body);

    // Test node is removed from DOM
    expect(body.childNodes[0].wholeText).toBe("fooo");
  });

  test("Should replace Component with TextNode", () => {
    var body = document.body;

    // populate DOM
    var mountCount = 0,
      unmountCount = 0;
    var component = vcomp(
      () => {
        return mountCount++;
      },
      () => {
        return unmountCount++;
      },
    );
    miso.diff(null, component, body);

    // Test component was populated
    expect(body.childNodes.length).toBe(1);
    expect(mountCount).toBe(1);
    expect(unmountCount).toBe(0);

    // Replace component
    var textNode = vtext("fooo");
    miso.diff(component, textNode, body);

    // Test node is removed from DOM
    expect(body.childNodes[0].wholeText).toBe("fooo");
    expect(unmountCount).toBe(1);
  });

  test("Should replace Component with Node", () => {
    var body = document.body;

    // populate DOM
    var mountCount = 0,
      unmountCount = 0;
    var component = vcomp(
      () => {
        return mountCount++;
      },
      () => {
        return unmountCount++;
      },
    );
    miso.diff(null, component, body);

    // Test component was populated
    expect(body.childNodes.length).toBe(1);
    expect(mountCount).toBe(1);
    expect(unmountCount).toBe(0);

    // Replace component
    var node = vnode("div", []);
    miso.diff(component, node, body);

    // Test node is removed from DOM
    expect(body.children[0].tagName).toBe("DIV");
    expect(unmountCount).toBe(1);
  });

  test("Should replace TextNode with Node", () => {
    var body = document.body;

    // populate DOM
    var textNode = vtext("fooo");
    miso.diff(null, textNode, body);

    // Test node was populated
    expect(body.childNodes.length).toBe(1);

    // Replace node
    var node = vnode("div", []);
    miso.diff(textNode, node, body);

    // Test node is removed from DOM
    expect(body.children[0].tagName).toBe("DIV");
  });

  test("Should remove a DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = null;
    var newNode = vnode("div", []);
    miso.diff(currentNode, newNode, body);

    // Test node was populated
    expect(body.children.length).toBe(1);

    // Remove node
    miso.diff(newNode, null, body);

    // Test node is removed from DOM
    expect(body.children.length).toBe(0);
  });

  test("Should create a new property on a DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode("div", [], {
      id: "a",
    });
    miso.diff(null, currentNode, body);
    expect(currentNode.domRef["id"]).toBe("a");
  });

  test("Should skip if window miso.diffing identical properties", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode("div", [], {
      id: "a",
    });
    miso.diff(null, currentNode, body);

    var newNode = vnode("div", [], {
      id: "a",
    });
    miso.diff(currentNode, newNode, body);
    expect(currentNode.domRef).toBe(newNode.domRef);
  });

  test("Should create a custom attribute on a DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode(
      "div",
      [],
      {
        lol: "lol",
      },
      {},
    );
    miso.diff(null, currentNode, body);
    expect(currentNode.domRef.getAttribute("lol")).toBe("lol");
  });

  test("Should change a custom attribute on a DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode(
      "div",
      [],
      {
        lol: "lol",
      },
      {},
    );
    miso.diff(null, currentNode, body);
    expect(currentNode.domRef.getAttribute("lol")).toBe("lol");

    var newNode = vnode(
      "div",
      [],
      {
        lol: "lolz",
      },
      {},
    );
    miso.diff(currentNode, newNode, body);
    expect(currentNode.domRef.getAttribute("lol")).toBe("lolz");
  });

  test("Should remove a custom attribute from a DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode("div", [], {
      lol: "lol",
    });
    miso.diff(null, currentNode, body);
    expect(currentNode.domRef.getAttribute("lol")).toBe("lol");

    // test property change
    var newNode = vnode("div", [], {});
    miso.diff(currentNode, newNode, body);
    expect(newNode.domRef.getAttribute("lol")).toBe(null);
  });

  test("Should remove a property from DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode("div", [], {
      id: "someid",
    });
    miso.diff(null, currentNode, body);

    // test property change
    var newNode = vnode("div", [], {});
    miso.diff(currentNode, newNode, body);
    expect(newNode.domRef["id"]).toBe("");
  });

  test("Should change a property from DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode("div", [], {
      id: "someid",
    });
    miso.diff(null, currentNode, body);

    // test property change
    var newNode = vnode("div", [], {
      id: "foo",
    });
    miso.diff(currentNode, newNode, body);
    expect(newNode.domRef["id"]).toBe("foo");
  });

  test("Should create css on a DOM node", () => {
    var body = document.body;

    // populate DOM
    var newNode = vnode(
      "div",
      [],
      {},
      {
        color: "red",
      },
    );
    miso.diff(null, newNode, body);
    expect(newNode.domRef.style["color"]).toBe("red");
  });

  test("Should remove css from DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode(
      "div",
      [],
      {},
      {
        color: "red",
      },
    );
    miso.diff(null, currentNode, body);

    // test css change
    var newNode = vnode("div", [], {}, {});
    miso.diff(currentNode, newNode, body);
    expect(newNode.domRef.style["color"]).toBe("");
  });

  test("Should change css on a DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode(
      "div",
      [],
      {},
      {
        color: "red",
      },
    );
    miso.diff(null, currentNode, body);

    // test css change
    var newNode = vnode(
      "div",
      [],
      {},
      {
        color: "blue",
      },
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.domRef.style["color"]).toBe("blue");
  });

  test("Should no-op change to css on a DOM node", () => {
    var body = document.body;

    // populate DOM
    var currentNode = vnode(
      "div",
      [],
      {},
      {
        color: "red",
      },
    );
    miso.diff(null, currentNode, body);

    // test css no-op change
    var newNode = vnode(
      "div",
      [],
      {},
      {
        color: "red",
      },
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.domRef.style["color"]).toBe("red");
  });

  test("Should call onCreated and onDestroyed", () => {
    var body = document.body;

    // populate DOM
    var create = 0,
      destroy = 0;
    var currentNode = vnode(
      "div",
      [],
      {},
      {},
      "html",
      null,
      function () {
        create++;
      },
      function () {
        destroy++;
      },
      null,
      "key",
    );

    miso.diff(null, currentNode, body);
    expect(create).toBe(1);

    miso.diff(currentNode, null, body);
    expect(destroy).toBe(1);
  });

  test("Should call onCreated and onBeforeDestroyed", () => {
    var body = document.body;

    // populate DOM
    var create = 0,
      destroy = 0;
    var currentNode = vnode(
      "div",
      [],
      {},
      {},
      "html",
      null,
      function () {
        create++;
      },
      null,
      function () {
        destroy++;
      },
      "key",
    );

    miso.diff(null, currentNode, body);
    expect(create).toBe(1);

    miso.diff(currentNode, null, body);
    expect(destroy).toBe(1);
  });

  test("Should call onDestroyed recursively", () => {
    var body = document.body;
    // populate DOM
    var destroy = 0,
      childDestroy = 0;
    var currentNode = vnode(
      "div",
      [
        vnode(
          "div",
          [],
          {},
          {},
          "html",
          null,
          null,
          function () {
            childDestroy++;
          },
          null,
          "a",
        ),
      ],
      {},
      {},
      "html",
      null,
      null,
      function () {
        destroy++;
      },
      null,
      "b",
    );
    miso.diff(null, currentNode, body);
    miso.diff(currentNode, null, body);
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
  });

  test("Should call onBeforeDestroyed recursively", () => {
    var body = document.body;
    // populate DOM
    var destroy = 0;
    var childDestroy = 0;
    var currentNode = vnode(
      "div",
      [
        vnode(
          "div",
          [],
          {},
          {},
          "html",
          null,
          null,
          null,
          function () {
            childDestroy++;
          },
          "a",
        ),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      function () {
        destroy++;
      },
      "b",
    );
    miso.diff(null, currentNode, body);
    miso.diff(currentNode, null, body);
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
  });

  test("Should recreate a DOM node when tags are the same but keys are window miso.different", () => {
    var body = document.body;
    var destroy = 0;
    var currentNode = vnode(
      "div",
      [],
      {},
      {},
      "html",
      null,
      null,
      function () {
        destroy++;
      },
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [],
      {},
      {},
      "html",
      null,
      null,
      function () {
        destroy++;
      },
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    expect(destroy).toBe(0);
    miso.diff(currentNode, newNode, body);
    var newKeyedNode = vnode(
      "div",
      [],
      {},
      {},
      "html",
      null,
      null,
      function () {
        destroy++;
      },
      null,
      "key-2",
    );
    miso.diff(currentNode, newKeyedNode, body);
    expect(destroy).toBe(1);
  });

  test("Should execute left-hand side happy path key-window miso.diffing case", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [vnodeKeyed("div", "a"), vnodeKeyed("div", "b"), vnodeKeyed("div", "c")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [vnodeKeyed("div", "a"), vnodeKeyed("div", "b"), vnodeKeyed("div", "c")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should miso.diff keys properly when keys are prepended", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [vnodeKeyed("div", "1")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [vnodeKeyed("div", "2"), vnodeKeyed("div", "1")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should execute right-hand side happy path key-window miso.diffing case", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [vnodeKeyed("div", "a"), vnodeKeyed("div", "c")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [vnodeKeyed("div", "z"), vnodeKeyed("div", "c")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should swap nodes", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [vnodeKeyed("div", "a"), vnodeKeyed("div", "b")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [vnodeKeyed("div", "b"), vnodeKeyed("div", "a")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should execute flip-flop case", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [vnodeKeyed("div", "a"), vnodeKeyed("div", "b"), vnodeKeyed("div", "c")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [vnodeKeyed("div", "c"), vnodeKeyed("div", "b"), vnodeKeyed("div", "a")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    for (var i = 0; i < 3; i++) {
      expect(currentNode.children[i]).not.toBe(undefined);
      expect(newNode.children[i]).not.toBe(undefined);
    }
  });

  test("Should execute swapped case on 1k nodes", () => {
    var body = document.body;
    var kids = [];
    for (var i = 1; i < 1001; i++) kids.push(vnodeKeyed("div", i));

    var currentNode = vnode(
      "div",
      kids,
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );

    var newKids = [];
    for (i = 1; i < 1001; i++) {
      if (i == 3) {
        newKids.push(vnodeKeyed("div", 999));
      } else if (i == 999) {
        newKids.push(vnodeKeyed("div", 3));
      } else {
        newKids.push(vnodeKeyed("div", i));
      }
    }
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      newKids,
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(1000);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    for (i = 0; i < 1000; i++) {
      expect(newNode.children[i].key).toBe(currentNode.children[i].key);
      expect(newNode.children[i].children[0].text).toBe(
        currentNode.children[i].children[0].text,
      );
      expect(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
      expect(newNode.children[i].domRef).not.toBe(undefined);
      expect(currentNode.children[i].domRef).not.toBe(undefined);
    }
  });

  test("Should execute top-left and bottom-right match case", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [
        vnodeKeyed("div", "d"),
        vnodeKeyed("div", "a"),
        vnodeKeyed("div", "k"),
        vnodeKeyed("div", "r"),
        vnodeKeyed("div", "b"),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [
        vnodeKeyed("div", "a"),
        vnodeKeyed("div", "b"),
        vnodeKeyed("div", "r"),
        vnodeKeyed("div", "k"),
        vnodeKeyed("div", "d"),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(5);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should handle duplicate keys case", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [
        vnodeKeyed("div", "a"),
        vnodeKeyed("div", "a"),
        vnodeKeyed("div", "a"),
        vnodeKeyed("div", "b"),
        vnodeKeyed("div", "b"),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [
        vnodeKeyed("div", "b"),
        vnodeKeyed("div", "b"),
        vnodeKeyed("div", "b"),
        vnodeKeyed("div", "a"),
        vnodeKeyed("div", "a"),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(5);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should execute top-right and bottom-left match case", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [
        vnodeKeyed("div", "d"),
        vnodeKeyed("div", "a"),
        vnodeKeyed("div", "g"),
        vnodeKeyed("div", "b"),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [
        vnodeKeyed("div", "b"),
        vnodeKeyed("div", "g"),
        vnodeKeyed("div", "d"),
        vnodeKeyed("div", "a"),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(4);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Nothing matches case", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [vnodeKeyed("div", "e"), vnodeKeyed("div", "k"), vnodeKeyed("div", "l")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [vnodeKeyed("div", "b"), vnodeKeyed("div", "z"), vnodeKeyed("div", "j")],
      {},
      {},
      "html",
      null,
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should handle nothing matches case where new key is found in old map", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [
        vnodeKeyed("div", "a"),
        vnodeKeyed("div", "k"),
        vnodeKeyed("div", "l"),
        vnodeKeyed("div", "c"),
        vnodeKeyed("div", "g"),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [
        vnodeKeyed("div", "b"),
        vnodeKeyed("div", "c"),
        vnodeKeyed("div", "l"),
        vnodeKeyed("div", "r"),
        vnodeKeyed("div", "k"),
      ],
      {},
      {},
      "html",
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(5);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should append new nodes in keys patch", () => {
    var body = document.body;
    var currentNode = vnode(
      "div",
      [vnodeKeyed("div", "a")],
      {},
      {},
      "html",
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(null, currentNode, body);
    var newNode = vnode(
      "div",
      [vnodeKeyed("div", "a"), vnodeKeyed("div", "c"), vnodeKeyed("div", "k")],
      {},
      {},
      "html",
      null,
      null,
      null,
      "key-1",
    );
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test("Should window miso.diff keyed text nodes", () => {
    var body = document.body;
    var currentNode = vnodeKids("div", [
      vtextKeyed("foo", 1),
      vtextKeyed("bar", 2),
      vtextKeyed("baz", 3),
    ]);
    miso.diff(null, currentNode, body);
    var newNode = vnodeKids("div", [
      vtextKeyed("baz", 3),
      vtextKeyed("bar", 2),
      vtextKeyed("foo", 1),
    ]);
    miso.diff(currentNode, newNode, body);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(newNode.children).toEqual(currentNode.children);
  });

  test("Should copy simple nested DOM into VTree", () => {
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var nestedDiv = document.createElement("div");
    div.appendChild(nestedDiv);
    var txt = document.createTextNode("foo");
    nestedDiv.appendChild(txt);
    var currentNode = vnodeKids("div", [vnodeKids("div", [vtext("foo")])]);
    miso.copyDOMIntoVTree(true, body, currentNode);
    expect(currentNode.children[0].children[0].text).toEqual("foo");
  });

  test("Should fail because of expecting text node", () => {
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var nestedDiv = document.createElement("div");
    div.appendChild(nestedDiv);
    var currentNode = vnodeKids("div", [vtext("foo")]);
    var res = miso.copyDOMIntoVTree(true, body, currentNode);
    expect(res).toEqual(false);
  });

  test("Should fail because of expecting element", () => {
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foo");
    div.appendChild(txt);
    var currentNode = vnodeKids("div", [vnode("div", [])]);
    var res = miso.copyDOMIntoVTree(true, body, currentNode);
    expect(res).toEqual(false);
  });

  test("Should fail because of non-matching text", () => {
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foo");
    div.appendChild(txt);
    var currentNode = vnodeKids("div", [vtext("bar")]);
    var res = miso.copyDOMIntoVTree(true, body, currentNode);
    expect(res).toEqual(false);
  });

  test("Should fail because of non-matching DOM and VDOM", () => {
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foobar");
    div.appendChild(txt);
    var currentNode = vnodeKids("div", [vtext("foo")]);
    var res = miso.copyDOMIntoVTree(true, body, currentNode);
    expect(res).toEqual(false);
  });

  test("Should copy DOM into VTree with multiple consecutive text nodes and collapse them", () => {
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foobarbaz");
    div.appendChild(txt);
    var currentNode = vnodeKids("div", [
      vtext("foo"),
      vtext("bar"),
      vtext("baz"),
    ]);
    miso.copyDOMIntoVTree(true, body, currentNode);
    // Expect "foobarbaz" to be split up into three nodes in the DOM
    expect(div.childNodes[0].textContent).toEqual("foobarbaz");
  });

  test("Should copy DOM into VTree with multiple consecutive text nodes and collapse them without mount point", () => {
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foobarbaz");
    div.appendChild(txt);
    var currentNode = vnodeKids("div", [
      vtext("foo"),
      vtext("bar"),
      vtext("baz"),
      vnodeKids("div", []),
      vtext("foo"),
      vtext("bar"),
      vtext("baz"),
    ]);
    miso.copyDOMIntoVTree(true, null, currentNode);
    // Expect "foobarbaz" to be split up into three nodes in the DOM
    expect(div.childNodes[0].textContent).toEqual("foobarbaz");
    expect(div.childNodes[2].textContent).toEqual("foobarbaz");
  });

  test("Should copy DOM into VTree at mountPoint", () => {
    var body = document.body;
    var unrelatedDiv = document.createElement("div");
    body.appendChild(document.createElement("script"));
    body.appendChild(document.createTextNode("test"));
    body.appendChild(unrelatedDiv);
    var unrelatedTxt = document.createTextNode("Not part of Miso app");
    unrelatedDiv.appendChild(unrelatedTxt);
    var misoDiv = document.createElement("div");
    body.appendChild(misoDiv);
    var nestedDiv1 = document.createElement("div");
    misoDiv.appendChild(nestedDiv1);
    var nestedDiv2 = document.createElement("div");
    nestedDiv1.appendChild(nestedDiv2);
    var txt = document.createTextNode("foo");
    nestedDiv2.appendChild(txt);
    var currentNode = vnodeKids("div", [vnodeKids("div", [vtext("foo")])]);
    var succeeded = miso.copyDOMIntoVTree(true, misoDiv, currentNode);
    expect(currentNode.children[0].children[0].domRef).toEqual(txt);
    expect(succeeded).toEqual(true);
  });

  test("Should copy DOM into VTree at body w/ script / text siblings", () => {
    var body = document.body;
    var unrelatedDiv = document.createElement("div");
    body.appendChild(document.createElement("script"));
    body.appendChild(document.createTextNode("test"));
    body.appendChild(unrelatedDiv);
    var unrelatedTxt = document.createTextNode("Not part of Miso app");
    unrelatedDiv.appendChild(unrelatedTxt);
    var misoDiv = document.createElement("div");
    body.appendChild(misoDiv);
    var nestedDiv1 = document.createElement("div");
    misoDiv.appendChild(nestedDiv1);
    var nestedDiv2 = document.createElement("div");
    nestedDiv1.appendChild(nestedDiv2);
    var txt = document.createTextNode("foo");
    nestedDiv2.appendChild(txt);
    var currentNode = vnodeKids("div", [vnodeKids("div", [vtext("foo")])]);
    var succeeded = miso.copyDOMIntoVTree(true, body, currentNode);
    expect(currentNode.children[0].children[0].domRef).toEqual(txt);
    expect(succeeded).toEqual(false);
  });

  // dmj: jsdom catches this, not possible to run
  // ● Should fail to mount on a text node
  //   HierarchyRequestError: Node can't be inserted in a #text parent.
  //
  // test('Should fail to mount on a text node', () => {
  //
  //   var body = document.body;
  //   var misoTxt = document.createTextNode("foo");
  //   body.appendChild(misoTxt);
  //   var currentNode = vnodeKids('div', [ vnodeKids('div', [ vtext("foo") ]) ]);
  //   var succeeded = miso.copyDOMIntoVTree(true, misoTxt, currentNode);
  //   expect(succeeded).toEqual(false);
  // });

  test("Should mount on an empty body", () => {
    var currentNode = vnodeKids("div", [vnodeKids("div", [vtext("foo")])]);
    var succeeded = miso.copyDOMIntoVTree(true, null, currentNode);
    expect(succeeded).toEqual(false);
  });

  test("Should pass integrity check", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: {},
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: {},
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
  });

  test("Should fail integrity check on bad tag", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: {},
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: {},
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.tag = "lol";
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on bad tag in copyDOMIntoVTree w/ logging enabled", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "lol",
      props: {},
      children: [{ type: "vtext", text: "fool?" }],
      key: null,
      ns: "HTML",
      css: {},
    };
    var result = miso.copyDOMIntoVTree(true, body, vtree);
    expect(result).toEqual(false);
  });

  test("Should fail integrity check on miso.differing vtext", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: {},
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: {},
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.children[0].text = "oops";
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on miso.differing child lengths", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: {},
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: {},
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.children = [];
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(false);
  });

  test("Should fail integrity check on miso.differing styles", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    child.style["background-color"] = "red";
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: {},
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: { "background-color": "red" },
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.css["background-color"] = "green";
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on miso.differing styles, for color", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    child.style["background-color"] = "red";
    child.style["color"] = "#cccccc";
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: {},
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: { "background-color": "red", color: "#cccccc" },
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.css["color"] = "#dddddd";
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on miso.differing props", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    child.style["background-color"] = "red";
    child.className = "something";
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: { class: "something" },
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: { "background-color": "red" },
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.props["class"] = "something-else";
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on differing height / width", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    child.style["background-color"] = "red";
    child.className = "something";
    child.height = "100";
    child.width = "100";
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: { class: "something", height: "100", width: "100" },
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: { "background-color": "red" },
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.props["height"] = "200";
    vtree.props["width"] = "200";
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on random property (title)", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    child["title"] = "bar";
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: { title: "bar" },
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: {},
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.props["title"] = "woz";
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on href", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    child.style["background-color"] = "red";
    child.href = "google.com";
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: { href: "google.com" },
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: { "background-color": "red" },
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.props["href"] = "notgoogle.com";
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on vtext domRef", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    child.style["background-color"] = "red";
    child.href = "google.com";
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: { href: "google.com" },
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: { "background-color": "red" },
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(1);
    vtree.children[0].domRef = document.createElement("div");
    check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should fail integrity check on unknown property test", () => {
    var document = window.document;
    var body = document.body;
    var child = document.createElement("div");
    var misoTxt = document.createTextNode("foo");
    body.appendChild(child);
    child.appendChild(misoTxt);
    var vtree = {
      type: "vnode",
      domRef: null,
      tag: "div",
      props: { foobah: "lol" },
      children: [{ type: "vtext", text: "foo" }],
      key: null,
      ns: "HTML",
      css: {},
    };
    var result = miso.copyDOMIntoVTree(false, body, vtree);
    expect(result).toEqual(true);
    var check = miso.integrityCheck(true, vtree);
    expect(check).toBe(0);
  });

  test("Should set body[data-component-id] via setBodyComponent()", () => {
    var document = window.document;
    miso.setBodyComponent("component-one");
    expect(document.body.getAttribute("data-component-id")).toEqual(
      "component-one",
    );
  });

  test("Should call callFocus() and callBlur()", () => {
    var document = window.document;
    var child = document.createElement("input");
    child["id"] = "foo";
    document.body.appendChild(child);
    miso.callFocus("blah", document, 0); /* missing case */
    miso.callFocus("foo", document, 0); /* found case */
    miso.callFocus("foo", document, 1); /* found case */
    expect(document.activeElement).toEqual(child);
    miso.callBlur("blah", document, 0); /* missing case */
    miso.callBlur("foo", document, 0); /* found case */
    miso.callBlur("foo", document, 1); /* found case */
    expect(document.activeElement).toEqual(document.body);
  });

  test("Should delegate and undelegate button click", () => {
    var document = window.document;
    var body = document.body;
    var count = 0;
    var result = null;
    var events = {
      click: {
        runEvent: function (e) {
          result = miso.eventJSON([[]], e);
          count++;
        },
        options: {
          preventDefault: true,
          stopPropagation: false,
        },
      },
    };
    var vtreeChild = {
      type: "vnode",
      tag: "button",
      ns: "HTML",
      props: {},
      children: [],
      css: {},
      events: events,
    };
    var vtree = {
      type: "vnode",
      tag: "div",
      ns: "HTML",
      props: {},
      children: [vtreeChild],
      css: {},
      events: events,
    };

    /* initial page draw */
    miso.diff(null, vtree, document.body);

    /* ensure structures match */
    expect(vtree.domRef).toEqual(document.body.childNodes[0]);
    expect(vtreeChild.domRef).toEqual(
      document.body.childNodes[0].childNodes[0],
    );

    /* setup event delegation */
    events = { click: ["click", true] };
    var getVTree = function (cb) {
      cb(vtree);
    };
    miso.delegate(body, events, getVTree, true);

    /* initiate click event */
    vtreeChild.domRef.click();

    /* check results */
    expect(count).toEqual(2);
    expect(result).not.toEqual(null);

    /* unmount delegation */
    miso.undelegate(document.body, events, getVTree, true);
  });

  test("Should unmount recursively in order", () => {
    var document = window.document;
    var unmounts = [];
    var mkVComp = function (name, children) {
      return {
        type: "vcomp",
        tag: "div",
        props: {},
        children: children,
        "data-component-id": name,
        css: {},
        ns: "HTML",
        mount: function () {},
        unmount: function () {
          unmounts.push(name);
        },
      };
    };

    var vtree = mkVComp("one", [mkVComp("two", [mkVComp("three", [])])]);
    miso.diff(null, vtree, document.body);
    miso.diff(vtree, null, document.body);
    expect(unmounts).toEqual(["one", "two", "three"]);
  });

  test("Should be latest version", () => {
    expect(miso.version).toEqual("1.9.0.0");
  });
});
