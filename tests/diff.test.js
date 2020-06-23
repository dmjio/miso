const diff = require('./diff');
const isomorphic = require('./isomorphic.js');
const jsdom = require('jsdom');

function vnode(tag, children, props, css, ns, ref, oc, od, bd, key) {
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
        'onBeforeDestroyed': bd,
        'key': key
    };
}

function vnodeKeyed(tag, key) {
    return {
        'type': 'vnode',
        'tag': tag,
        'children': [vtext(key)],
        'props': {},
        'css': {},
        'ns': 'HTML',
        'domRef': null,
        'onCreated': null,
        'onDestroyed': null,
        'onBeforeDestroyed': null,
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
        'onBeforeDestroyed': null,
    };
}

function vtext(txt) {
    return {
        'type': 'vtext',
        'text': txt
    };
}

function vtextKeyed(txt, key) {
    return {
        'type': 'vtext',
        'text': txt,
	'key': key
    };
}


// base case
test('Should be null when diffing two null virtual DOMs', () => {
    const document = new jsdom.JSDOM().window.document;
    const body = document.body;
    var c = null;
    n = null;
    window['diff'](c, n, body, document);
    expect(body.childNodes.length).toBe(0);
});

test('Should create a new text node', () => {
    const doc = new jsdom.JSDOM().window.document;
    const body = doc.body;
    var newNode = {
        'type': 'vtext',
        'text': 'foo'
    };
    window['diff'](null, newNode, body, doc);
    expect(newNode.domRef.wholeText).toBe('foo');
});

test('Should window diff two identical text nodes', () => {
    const doc = new jsdom.JSDOM().window.document;
    const body = doc.body;
    var currentNode = {
        'type': 'vtext',
        'text': 'foo'
    };
    window['diff'](null, currentNode, body, doc);
    expect(currentNode.domRef.wholeText).toBe('foo');
    var newNode = {
        'type': 'vtext',
        'text': 'foo'
    };
    window['diff'](currentNode, newNode, body, doc);
    expect('foo').toBe(newNode.domRef.wholeText);
});

test('Should window diff two window different text nodes', () => {
    const doc = new jsdom.JSDOM().window.document;
    const body = doc.body;
    var currentNode = {
        'type': 'vtext',
        'text': 'foo'
    };
    window['diff'](null, currentNode, body, doc);
    expect(currentNode.domRef.wholeText).toBe('foo');
    var newNode = {
        'type': 'vtext',
        'text': 'bar'
    };
    window['diff'](currentNode, newNode, body, doc);
    expect(newNode.domRef.wholeText).toBe('bar')
});

test('Should create a new DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('div', []);
    window['diff'](currentNode, newNode, body, document);
    expect(body.children[0]).toBe(newNode.domRef);
});

test('Should create an SVG DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('div', [], {}, {}, 'svg');
    window['diff'](currentNode, newNode, body, document);
    expect(body.children[0]).toBe(newNode.domRef);
});

test('Should create a MathML DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('math', [], {}, {}, 'mathml');
    window['diff'](currentNode, newNode, body, document);
    expect(body.children[0]).toBe(newNode.domRef);
});

test('Should create an SVG DOM node, with href attribute', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('ellipse', [], {
        'href': 'https://google.com'
    }, {}, 'svg');
    window['diff'](currentNode, newNode, body, document);
    expect(body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", 'href')).toBe('https://google.com');
});

test('Should create an SVG DOM node, with href attribute, and change it', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('ellipse', [], {
        'href': 'https://google.com'
    }, {}, 'svg');
    window['diff'](currentNode, newNode, body, document);
    expect(body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", 'href')).toBe('https://google.com');
    var newerNode = vnode('ellipse', [], {
        'href': 'https://yahoo.com'
    }, {}, 'svg');
    window['diff'](newNode, newerNode, body, document);
    expect(body.children[0].getAttributeNS("http://www.w3.org/1999/xlink", 'href')).toBe('https://yahoo.com');
});

test('Should create an SVG DOM node, with regular attribute', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('ellipse', [], {
        'rx': '100'
    }, {}, 'svg');
    window['diff'](currentNode, newNode, body, document);
    expect(body.children[0].getAttribute('rx')).toBe('100');
});

test('Should create an SVG DOM node, with regular attribute, and change it', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = null;
    var newNode = vnode('ellipse', [], {
        'rx': '100'
    }, {}, 'svg');
    window['diff'](currentNode, newNode, body, document);
    expect(body.children[0].getAttribute('rx')).toBe('100');
    var newerNode = vnode('ellipse', [], {
        'rx': '200'
    }, {}, 'svg');
    window['diff'](newNode, newerNode, body, document);
    expect(body.children[0].getAttribute('rx')).toBe('200');
});

test('Should replace a Node with a new Node of a window different tag', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var node = vnode('div', []);
    window['diff'](null, node, body, document);

    // Test node was populated
    expect(body.children.length).toBe(1);

    // Replace node
    newNode = vnode('a', []);
    window['diff'](node, newNode, body, document);

    // Test node is removed from DOM
    expect(body.children[0].tagName).toBe('A');
});

test('Should create children', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var node = vnode('div', [vnode('div', [])]);
    window['diff'](null, node, body, document);
    expect(node.domRef.children.length).toBe(1);
});

test('Should remove a child', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var node = vnode('div', [vnode('div', [])]);
    window['diff'](null, node, body, document);
    expect(node.domRef.children.length).toBe(1);

    // populate DOM
    var newNode = vnode('div', []);
    window['diff'](node, newNode, body, document);
    expect(node.domRef.children.length).toBe(0);
});


test('Should replace Node with TextNode', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var node = vnode('div', []);
    window['diff'](null, node, body, document);

    // Test node was populated
    expect(body.childNodes.length).toBe(1);

    // Replace node
    textNode = vtext('fooo');
    window['diff'](node, textNode, body, document);

    // Test node is removed from DOM
    expect(body.childNodes[0].wholeText).toBe('fooo');
});

test('Should replace TextNode with Node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var textNode = vtext('fooo');
    window['diff'](null, textNode, body, document);

    // Test node was populated
    expect(body.childNodes.length).toBe(1);

    // Replace node
    node = vnode('div', []);
    window['diff'](textNode, node, body, document);

    // Test node is removed from DOM
    expect(body.children[0].tagName).toBe('DIV');
});

test('Should remove a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = null;
    var newNode = vnode('div', []);
    window['diff'](currentNode, newNode, body, document);

    // Test node was populated
    expect(body.children.length).toBe(1);

    // Remove node
    window['diff'](newNode, null, body, document);

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
    window['diff'](null, currentNode, body, document)
    expect(currentNode.domRef['id']).toBe('a');
});

test('Should skip if window diffing identical properties', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'id': 'a'
    });
    window['diff'](null, currentNode, body, document)

    var newNode = vnode('div', [], {
        'id': 'a'
    });
    window['diff'](currentNode, newNode, body, document)
    expect(currentNode.domRef).toBe(newNode.domRef);
});

test('Should create a custom attribute on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'lol': 'lol'
    }, {});
    window['diff'](null, currentNode, body, document)
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');
});

test('Should change a custom attribute on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'lol': 'lol'
    }, {});
    window['diff'](null, currentNode, body, document)
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');

    var newNode = vnode('div', [], {
        'lol': 'lolz'
    }, {});
    window['diff'](currentNode, newNode, body, document)
    expect(currentNode.domRef.getAttribute('lol')).toBe('lolz');
});

test('Should remove a custom attribute from a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'lol': 'lol'
    });
    window['diff'](null, currentNode, body, document)
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');

    // test property change
    var newNode = vnode('div', [], {});
    window['diff'](currentNode, newNode, body, document);
    expect(newNode.domRef.getAttribute('lol')).toBe(null);
});

test('Should remove a property from DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'id': 'someid'
    });
    window['diff'](null, currentNode, body, document)

    // test property change
    var newNode = vnode('div', [], {});
    window['diff'](currentNode, newNode, body, document);
    expect(newNode.domRef['id']).toBe('');
});

test('Should change a property from DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {
        'id': 'someid'
    });
    window['diff'](null, currentNode, body, document)

    // test property change
    var newNode = vnode('div', [], {
        'id': 'foo'
    });
    window['diff'](currentNode, newNode, body, document);
    expect(newNode.domRef['id']).toBe('foo');
});

test('Should create css on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var newNode = vnode('div', [], {}, {
        'color': 'red'
    });
    window['diff'](null, newNode, body, document)
    expect(newNode.domRef.style['color']).toBe('red');
});

test('Should remove css from DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {}, {
        'color': 'red'
    });
    window['diff'](null, currentNode, body, document)

    // test css change
    var newNode = vnode('div', [], {}, {});
    window['diff'](currentNode, newNode, body, document);
    expect(newNode.domRef.style['color']).toBe('');
});

test('Should change css on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {}, {
        'color': 'red'
    });
    window['diff'](null, currentNode, body, document)

    // test css change
    var newNode = vnode('div', [], {}, {
        'color': 'blue'
    });
    window['diff'](currentNode, newNode, body, document);
    expect(newNode.domRef.style['color']).toBe('blue');
});

test('Should no-op change to css on a DOM node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var currentNode = vnode('div', [], {}, {
        'color': 'red'
    });
    window['diff'](null, currentNode, body, document)

    // test css no-op change
    var newNode = vnode('div', [], {}, {
        'color': 'red'
    });
    window['diff'](currentNode, newNode, body, document);
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
    }, null, 'key');

    window['diff'](null, currentNode, body, document)
    expect(create).toBe(1);

    window['diff'](currentNode, null, body, document)
    expect(destroy).toBe(1);
});

test('Should call onCreated and onBeforeDestroyed', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;

    // populate DOM
    var create = 0;
    destroy = 0;
    var currentNode = vnode('div', [], {}, {}, "html", null, function() {
        create++;
    }, null, function() {
        destroy++;
    }, 'key');

    window['diff'](null, currentNode, body, document)
    expect(create).toBe(1);

    window['diff'](currentNode, null, body, document)
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
        }, null, 'a')], {}, {}, "html", null, null, function() {
            destroy++;
        }, null, 'b');
    window['diff'](null, currentNode, body, document)
    window['diff'](currentNode, null, body, document)
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
});

test('Should call onBeforeDestroyed recursively', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    // populate DOM
    var destroy = 0;
    childDestroy = 0;
    var currentNode =
        vnode('div', [vnode('div', [], {}, {}, "html", null, null, null, function() {
            childDestroy++;
        }, 'a')], {}, {}, "html", null, null, null, function() {
            destroy++;
        }, 'b');
    window['diff'](null, currentNode, body, document)
    window['diff'](currentNode, null, body, document)
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
});


test('Should recreate a DOM node when tags are the same but keys are window different', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [], {}, {}, "html", null, null, function() {
            destroy++;
        }, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [], {}, {}, "html", null, null, function() {
            destroy++;
        }, null, "key-1");
    window['diff'](null, currentNode, body, document)
    expect(destroy).toBe(0);
    window['diff'](currentNode, newNode, body, document)
    var newKeyedNode =
        vnode('div', [], {}, {}, "html", null, null, function() {
            destroy++;
        }, null, "key-2");
    window['diff'](currentNode, newKeyedNode, body, document)
    expect(destroy).toBe(1);
});

test('Should execute left-hand side happy path key-window diffing case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should diff keys properly when keys are prepended', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', '1')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
	vnode('div', [vnodeKeyed('div', '2'), vnodeKeyed('div', '1')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should execute right-hand side happy path key-window diffing case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'z'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});


test('Should swap nodes', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode = vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'a')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should execute flip-flop case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'c'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'a')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
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

test('Should execute swapped case on 1k nodes', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var kids = [];
    for (var i = 1; i < 1001; i++) kids.push(vnodeKeyed('div', i))

    var currentNode =  vnode('div', kids, {}, {}, "html", null, null, null, null, "key-1");

    var newKids = [];
    for (var i = 1; i < 1001; i++) {
	if (i == 3) {
            newKids.push(vnodeKeyed('div', 999))
	} else if (i == 999) {
            newKids.push(vnodeKeyed('div', 3))
	} else {
            newKids.push(vnodeKeyed('div', i))
	}
    }
    window['diff'](null, currentNode, body, document)
    var newNode =  vnode('div', newKids, {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(1000);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    for (var i = 0; i < 1000; i++) {
       expect(newNode.children[i].key).toBe(currentNode.children[i].key);
       expect(newNode.children[i].children[0].text).toBe(currentNode.children[i].children[0].text);
       expect(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
       expect(newNode.children[i].domRef).not.toBe(undefined);
       expect(currentNode.children[i].domRef).not.toBe(undefined);
    }
});

test('Should execute top-left and bottom-right match case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'd'), vnodeKeyed('div', 'a'), vnodeKeyed('div', 'k'), vnodeKeyed('div', 'r'), vnodeKeyed('div', 'b')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'r'), vnodeKeyed('div', 'k'), vnodeKeyed('div', 'd')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(5);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should handle duplicate keys case', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'a'), vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'b')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'a'), vnodeKeyed('div', 'a')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(5);
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
        vnode('div', [vnodeKeyed('div', 'd'), vnodeKeyed('div', 'a'), vnodeKeyed('div', 'g'), vnodeKeyed('div', 'b')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'g'), vnodeKeyed('div', 'd'), vnodeKeyed('div', 'a')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(4);
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
        vnode('div', [vnodeKeyed('div', 'e'), vnodeKeyed('div', 'k'), vnodeKeyed('div', 'l')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'z'), vnodeKeyed('div', 'j')], {}, {}, "html", null, null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
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
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'k'), vnodeKeyed('div', 'l'), vnodeKeyed('div', 'c'), vnodeKeyed('div', 'g')], {}, {}, "html", null, null, null, "key-1");
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c'), vnodeKeyed('div', 'l'), vnodeKeyed('div', 'r'), vnodeKeyed('div', 'k')], {}, {}, "html", null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(5);
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
    window['diff'](null, currentNode, body, document)
    var newNode =
        vnode('div', [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'c'), vnodeKeyed('div', 'k')], {}, {}, "html", null, null, null, "key-1");
    window['diff'](currentNode, newNode, body, document)
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(currentNode.children).toEqual(newNode.children);
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
});

test('Should window diff keyed text nodes', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var destroy = 0;
    var currentNode = vnodeKids('div', [ vtextKeyed ("foo",1), vtextKeyed ("bar",2), vtextKeyed ("baz",3)]);
    window['diff'](null, currentNode, body, document)
    var newNode = vnodeKids('div', [ vtextKeyed ("baz",3), vtextKeyed ("bar",2), vtextKeyed ("foo",1) ]);
    window['diff'](currentNode, newNode, body, document);
    expect(newNode.children.length).toBe(currentNode.children.length);
    expect(newNode.children).toEqual(currentNode.children);
});

test('Should copy simple nested DOM into VTree', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var nestedDiv = document.createElement("div");
    div.appendChild(nestedDiv);
    var txt = document.createTextNode("foo");
    nestedDiv.appendChild(txt);
    var currentNode = vnodeKids('div', [ vnodeKids('div', [ vtext("foo") ]) ]);
    window['copyDOMIntoVTree'](true, body, currentNode, document);
    expect(currentNode.children[0].children[0].text).toEqual('foo');
});

test('Should fail because of expecting text node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var nestedDiv = document.createElement("div");
    div.appendChild(nestedDiv);
    var currentNode = vnodeKids('div', [ vtext("foo") ]);
    var res = window['copyDOMIntoVTree'](true, body, currentNode, document);
    expect(res).toEqual(false);
});

test('Should fail because of expecting element', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foo");
    div.appendChild(txt);
    var currentNode = vnodeKids('div', [ vnode('div', []) ]);
    var res = window['copyDOMIntoVTree'](true, body, currentNode, document);
    expect(res).toEqual(false);
});

test('Should fail because of non-matching text', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foo");
    div.appendChild(txt);
    var currentNode = vnodeKids('div', [ vtext("bar") ]);
    var res = window['copyDOMIntoVTree'](true, body, currentNode, document);
    expect(res).toEqual(false);
});

test('Should fail because of non-matching DOM and VDOM', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foobar");
    div.appendChild(txt);
    var currentNode = vnodeKids('div', [ vtext("foo") ]);
    var res = window['copyDOMIntoVTree'](true, body, currentNode, document);
    expect(res).toEqual(false);
});

test('Should copy DOM into VTree with multiple consecutive text nodes and collapse them', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foobarbaz");
    div.appendChild(txt);
    var currentNode = vnodeKids('div', [ vtext("foo"), vtext("bar"), vtext("baz") ]);
    window['copyDOMIntoVTree'](true, body, currentNode, document);
    // Expect "foobarbaz" to be split up into three nodes in the DOM
    expect(div.childNodes[0].textContent).toEqual('foobarbaz');
});

test('Should copy DOM into VTree with multiple consecutive text nodes and collapse them without mount point', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var div = document.createElement("div");
    body.appendChild(div);
    var txt = document.createTextNode("foobarbaz");
    div.appendChild(txt);
    var currentNode = vnodeKids('div', [ vtext("foo"), vtext("bar"), vtext("baz"), vnodeKids('div',[]), vtext("foo"), vtext("bar"), vtext("baz"), ]);
    window['copyDOMIntoVTree'](true, null, currentNode, document);
    // Expect "foobarbaz" to be split up into three nodes in the DOM
    expect(div.childNodes[0].textContent).toEqual('foobarbaz');
    expect(div.childNodes[2].textContent).toEqual('foobarbaz');
});

test('Should copy DOM into VTree at mountPoint', () => {
    var document = new jsdom.JSDOM().window.document;
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
    var currentNode = vnodeKids('div', [ vnodeKids('div', [ vtext("foo") ]) ]);
    var succeeded = window['copyDOMIntoVTree'](true, misoDiv, currentNode, document);
    expect(currentNode.children[0].children[0].domRef).toEqual(txt);
    expect(succeeded).toEqual(true);
});

test('Should copy DOM into VTree at body w/ script / text siblings', () => {
    var document = new jsdom.JSDOM().window.document;
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
    var currentNode = vnodeKids('div', [ vnodeKids('div', [ vtext("foo") ]) ]);
    var succeeded = window['copyDOMIntoVTree'](true, body, currentNode, document);
    expect(currentNode.children[0].children[0].domRef).toEqual(txt);
    expect(succeeded).toEqual(false);
});

test('Should fail to mount on a text node', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var misoTxt = document.createTextNode("foo");
    body.appendChild(misoTxt);
    var currentNode = vnodeKids('div', [ vnodeKids('div', [ vtext("foo") ]) ]);
    var succeeded = window['copyDOMIntoVTree'](true, misoTxt, currentNode, document);
    expect(succeeded).toEqual(false);
});

test('Should mount on an empty body', () => {
    var document = new jsdom.JSDOM().window.document;
    var body = document.body;
    var currentNode = vnodeKids('div', [ vnodeKids('div', [ vtext("foo") ]) ]);
    var succeeded = window['copyDOMIntoVTree'](true, null, currentNode, document);
    expect(succeeded).toEqual(false);
});
