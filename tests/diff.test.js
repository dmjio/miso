const diff = require('./diff');
const jsdom = require('JSDom');

function vnode(tag, children, props, css, ns, ref, oc, od, key) {
    return {
        'type': 'vnode',
        'tag': tag,
        'children': children,
        'props': props,
        'css': css,
        'ns': ns,
        'domRef': ref,
        'onCreated': oc,
        'onDestroyed': od,
        'key': key
    };
}

function vnodeKeyed(tag, key) {
    return {
        'type': 'vnode',
        'tag': tag,
        'children': [],
        'props': {},
        'css': {},
        'ns': 'HTML',
        'domRef': null,
        'onCreated': null,
        'onDestroyed': null,
        'key': key
    };
}

function vnodeKids(tag, kids) {
    return {
        'type': 'vnode',
        'tag': tag,
        'children': kids,
        'props': {},
        'css': {},
        'ns': 'HTML',
        'domRef': null,
        'onCreated': null,
        'onDestroyed': null,
    };
}

function vtext(txt) {
    return {
        'type': 'vtext',
        'text': txt
    };
}

// base case
test('Should be null when diffing two null virtual DOMs', () => {
    const document = new jsdom.JSDOM().window.document;
    const body = document.body;
    var c = null;
    n = null;
    diff(c, n, body, document);
    expect(body.childNodes.length).toBe(0);
});

test('Should create a new text node', () => {
    const doc = new jsdom.JSDOM().window.document;
    const body = doc.body;
    var newNode = {
        'type': 'vtext',
        'text': 'foo'
    };
    diff(null, newNode, body, doc);
    expect(newNode.domRef.wholeText).toBe('foo');
});

test('Should diff two identical text nodes', () => {
    const doc = new jsdom.JSDOM().window.document;
    const body = doc.body;
    var currentNode = {
        'type': 'vtext',
        'text': 'foo'
    };
    diff(null, currentNode, body, doc);
    expect(currentNode.domRef.wholeText).toBe('foo');
    var newNode = {
        'type': 'vtext',
        'text': 'foo'
    };
    diff(currentNode, newNode, body, doc);
    expect('foo').toBe(newNode.domRef.wholeText);
});

test('Should diff two different text nodes', () => {
    const doc = new jsdom.JSDOM().window.document;
    const body = doc.body;
    var currentNode = {
        'type': 'vtext',
        'text': 'foo'
    };
    diff(null, currentNode, body, doc);
    expect(currentNode.domRef.wholeText).toBe('foo');
    var newNode = {
        'type': 'vtext',
        'text': 'bar'
    };
    diff(currentNode, newNode, body, doc);
    expect(newNode.domRef.wholeText).toBe('bar')
});

test('Should create a new DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('div', []);
    diff(currentNode, newNode, body, document);
    expect(body.children[0]).toBe(newNode.domRef);
});

test('Should create an SVG DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('div', [], {}, {}, 'svg');
    diff(currentNode, newNode, body, document);
    expect(body.children[0]).toBe(newNode.domRef);
});

test('Should create an SVG DOM node, with href attribute', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('ellipse', [], {
        'href': 'https://google.com'
    }, {}, 'svg');
    diff(currentNode, newNode, body, document);
    expect(body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", 'href')).toBe('https://google.com');
});

test('Should create an SVG DOM node, with href attribute, and change it', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('ellipse', [], {
        'href': 'https://google.com'
    }, {}, 'svg');
    diff(currentNode, newNode, body, document);
    expect(body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", 'href')).toBe('https://google.com');
    var newerNode = vnode('ellipse', [], {
        'href': 'https://yahoo.com'
    }, {}, 'svg');
    diff(newNode, newerNode, body, document);
    expect(body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", 'href')).toBe('https://yahoo.com');
});

test('Should create an SVG DOM node, with regular attribute', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('ellipse', [], {
        'rx': '100'
    }, {}, 'svg');
    diff(currentNode, newNode, body, document);
    expect(body.children[0].getAttribute('rx')).toBe('100');
});

test('Should create an SVG DOM node, with regular attribute, and change it', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('ellipse', [], {
        'rx': '100'
    }, {}, 'svg');
    diff(currentNode, newNode, body, document);
    expect(body.children[0].getAttribute('rx')).toBe('100');
    var newerNode = vnode('ellipse', [], {
        'rx': '200'
    }, {}, 'svg');
    diff(newNode, newerNode, body, document);
    expect(body.children[0].getAttribute('rx')).toBe('200');
});

test('Should replace a Node with a new Node of a different tag', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var node = vnode('div', []);
    diff(null, node, body, document);

    // Test node was populated
    expect(body.children.length).toBe(1);

    // Replace node
    newNode = vnode('a', []);
    diff(node, newNode, body, document);

    // Test node is removed from DOM
    expect(body.children[0].tagName).toBe('A');
});

test('Should create children', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var node = vnode('div', [vnode('div', [])]);
    diff(null, node, body, document);
    expect(node.domRef.children.length).toBe(1);
});

test('Should remove a child', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var node = vnode('div', [vnode('div', [])]);
    diff(null, node, body, document);
    expect(node.domRef.children.length).toBe(1);

    // populate DOM
    var newNode = vnode('div', []);
    diff(node, newNode, body, document);
    expect(node.domRef.children.length).toBe(0);
});


test('Should replace Node with TextNode', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var node = vnode('div', []);
    diff(null, node, body, document);

    // Test node was populated
    expect(body.childNodes.length).toBe(1);

    // Replace node
    textNode = vtext('fooo');
    diff(node, textNode, body, document);

    // Test node is removed from DOM
    expect(body.childNodes[0].wholeText).toBe('fooo');
});

test('Should replace TextNode with Node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var textNode = vtext('fooo');
    diff(null, textNode, body, document);

    // Test node was populated
    expect(body.childNodes.length).toBe(1);

    // Replace node
    node = vnode('div', []);
    diff(textNode, node, body, document);

    // Test node is removed from DOM
    expect(body.children[0].tagName).toBe('DIV');
});

test('Should remove a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = null;
    var newNode = vnode('div', []);
    diff(currentNode, newNode, body, document);

    // Test node was populated
    expect(body.children.length).toBe(1);

    // Remove node
    diff(newNode, null, body, document);

    // Test node is removed from DOM
    expect(body.children.length).toBe(0);
});

test('Should create a new property on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'id': 'a'
    });
    diff(null, currentNode, body, document)
    expect(currentNode.domRef['id']).toBe('a');
});

test('Should skip if diffing identical properties', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'id': 'a'
    });
    diff(null, currentNode, body, document)

    var newNode = vnode('div', [], {
        'id': 'a'
    });
    diff(currentNode, newNode, body, document)
    expect(currentNode.domRef).toBe(newNode.domRef);
});

test('Should create a custom attribute on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'lol': 'lol'
    }, {});
    diff(null, currentNode, body, document)
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');
});

test('Should change a custom attribute on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'lol': 'lol'
    }, {});
    diff(null, currentNode, body, document)
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');

    var newNode = vnode('div', [], {
        'lol': 'lolz'
    }, {});
    diff(currentNode, newNode, body, document)
    expect(currentNode.domRef.getAttribute('lol')).toBe('lolz');
});

test('Should remove a custom attribute from a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'lol': 'lol'
    });
    diff(null, currentNode, body, document)
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');

    // test property change
    var newNode = vnode('div', [], {});
    diff(currentNode, newNode, body, document);
    expect(newNode.domRef.getAttribute('lol')).toBe(null);
});

test('Should remove a property from DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'id': 'someid'
    });
    diff(null, currentNode, body, document)

    // test property change
    var newNode = vnode('div', [], {});
    diff(currentNode, newNode, body, document);
    expect(newNode.domRef['id']).toBe('');
});

test('Should change a property from DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'id': 'someid'
    });
    diff(null, currentNode, body, document)

    // test property change
    var newNode = vnode('div', [], {
        'id': 'foo'
    });
    diff(currentNode, newNode, body, document);
    expect(newNode.domRef['id']).toBe('foo');
});

test('Should create css on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var newNode = vnode('div', [], {}, {
        'color': 'red'
    });
    diff(null, newNode, body, document)
    expect(newNode.domRef.style['color']).toBe('red');
});

test('Should remove css from DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {}, {
        'color': 'red'
    });
    diff(null, currentNode, body, document)

    // test css change
    var newNode = vnode('div', [], {}, {});
    diff(currentNode, newNode, body, document);
    expect(newNode.domRef.style['color']).toBe('');
});

test('Should change css on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {}, {
        'color': 'red'
    });
    diff(null, currentNode, body, document)

    // test css change
    var newNode = vnode('div', [], {}, {
        'color': 'blue'
    });
    diff(currentNode, newNode, body, document);
    expect(newNode.domRef.style['color']).toBe('blue');
});

test('Should no-op change to css on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {}, {
        'color': 'red'
    });
    diff(null, currentNode, body, document)

    // test css no-op change
    var newNode = vnode('div', [], {}, {
        'color': 'red'
    });
    diff(currentNode, newNode, body, document);
    expect(newNode.domRef.style['color']).toBe('red');
});

test('Should call onCreated and onDestroyed', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var create = 0;
    destroy = 0;
    var currentNode = vnode('div', [], {}, {}, "html", null, function() {
        create++;
    }, function() {
        destroy++;
    });

    diff(null, currentNode, body, document)
    expect(create).toBe(1);

    diff(currentNode, null, body, document)
    expect(destroy).toBe(1);
});

test('Should call onDestroyed recursively', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    // populate DOM
    var destroy = 0;
    childDestroy = 0;
    var currentNode =
        vnode('div', [vnode('div', [], {}, {}, "html", null, null, function() {
            childDestroy++;
        })], {}, {}, "html", null, null, function() {
            destroy++;
        });
    diff(null, currentNode, body, document)
    diff(currentNode, null, body, document)
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
});

test('Should recreate a DOM node when tags are the same but keys are different', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [], {}, {}, "html", null, null, function() {
            destroy++;
        }, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [], {}, {}, "html", null, null, function() {
            destroy++;
        }, "key-1");
    diff(null, currentNode, body, document)
    expect(destroy).toBe(0);
    diff(currentNode, newNode, body, document)
    var newKeyedNode =
        vnode('div', [], {}, {}, "html", null, null, function() {
            destroy++;
        }, "key-2");
    diff(currentNode, newKeyedNode, body, document)
    expect(destroy).toBe(1);
});

test('Should execute left-hand side happy path key-diffing case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, "key-1");
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should execute right-hand side happy path key-diffing case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'z'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, "key-1");
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should execute flip-flop case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b')], {}, {}, "html", null, null, null, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'a')], {}, {}, "html", null, null, null, "key-1");
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should execute top-left and bottom-right match case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'd'), vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b')], {}, {}, "html", null, null, null, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'd')], {}, {}, "html", null, null, null, "key-1");
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should execute top-right and bottom-left match case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'd'), vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b')], {}, {}, "html", null, null, null, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'd'), vnodeKeyed('div', 'a')], {}, {}, "html", null, null, null, "key-1");
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Nothing matches case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'e'), vnodeKeyed('div', 'k'), vnodeKeyed('div', 'l')], {}, {}, "html", null, null, null, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'z'), vnodeKeyed('div', 'j')], {}, {}, "html", null, null, null, "key-1");
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should handle nothing matches case where new key is found in old map', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'k'), vnodeKeyed('div', 'c'), vnodeKeyed('div', 'g')], {}, {}, "html", null, null, null, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c'), vnodeKeyed('div', 'r'), vnodeKeyed('div', 'k')], {}, {}, "html", null, null, null, "key-1");
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(4);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should append new nodes in keys patch', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a')], {}, {}, "html", null, null, null, "key-1");
    diff(null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'c'), vnodeKeyed('div', 'k')], {}, {}, "html", null, null, null, "key-1");
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should not attempt to destroy text nodes recursively', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode = vnodeKids('div', [vtext ("derp")]);
    diff(null, currentNode, body, document)
    var newNode = vnodeKids('a', [vtext ("foo")]);
    diff(currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(newNode.domRef.childNodes[0].textContent).toBe("foo");
});
