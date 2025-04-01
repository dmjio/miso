"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var dom_1 = require("../miso/dom");
var smart_1 = require("../miso/smart");
var bun_test_1 = require("bun:test");
/* silence */
(0, bun_test_1.beforeAll)(function () {
    console.log = function () { };
    console.info = function () { };
    console.warn = function () { };
    console.error = function () { };
});
/* reset DOM */
(0, bun_test_1.afterEach)(function () {
    document.body.innerHTML = '';
});
(0, bun_test_1.describe)('DOM tests', function () {
    (0, bun_test_1.test)('Should be null when diffing two null virtual DOMs', function () {
        (0, dom_1.diff)(null, null, document.body);
        (0, bun_test_1.expect)(document.body.childNodes.length).toBe(0);
    });
    (0, bun_test_1.test)('Should create a new text node', function () {
        var newNode = (0, smart_1.vtext)('foo');
        (0, dom_1.diff)(null, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef.textContent).toBe('foo');
    });
    (0, bun_test_1.test)('Should window diff two identical text nodes', function () {
        var currentNode = (0, smart_1.vtext)('foo');
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef.textContent).toBe('foo');
        var newNode = (0, smart_1.vtext)('foo');
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)('foo').toBe(newNode.domRef.textContent);
    });
    (0, bun_test_1.test)('Should window diff two window different text nodes', function () {
        var currentNode = (0, smart_1.vtext)('foo');
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef.textContent).toBe('foo');
        var newNode = (0, smart_1.vtext)('bar');
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef.textContent).toBe('bar');
    });
    (0, bun_test_1.test)('Should create a new DOM node', function () {
        var newNode = (0, smart_1.vtree)();
        (0, dom_1.diff)(null, newNode, document.body);
        (0, bun_test_1.expect)(document.body.children[0]).toBe(newNode.domRef);
    });
    (0, bun_test_1.test)('Should create an SVG DOM node', function () {
        var currentNode = null;
        var newNode = (0, smart_1.vtree)({ ns: 'svg' });
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(document.body.children[0]).toBe(newNode.domRef);
    });
    (0, bun_test_1.test)('Should create a MathML DOM node', function () {
        var newNode = (0, smart_1.vtree)({ ns: 'mathml', tag: 'math' });
        (0, dom_1.diff)(null, newNode, document.body);
        (0, bun_test_1.expect)(document.body.children[0].namespaceURI).toBe('http://www.w3.org/1998/Math/MathML');
    });
    (0, bun_test_1.test)('Should create an SVG DOM node, with href attribute', function () {
        var tree = (0, smart_1.vtree)({
            tag: 'ellipse',
            ns: 'svg',
            props: {
                href: 'https://google.com',
            },
        });
        (0, dom_1.diff)(null, tree, document.body);
        (0, bun_test_1.expect)(document.body.children[0].getAttributeNS('http://www.w3.org/1999/xlink', 'href')).toBe('https://google.com');
    });
    (0, bun_test_1.test)('Should create an SVG DOM node, with href attribute, and change it', function () {
        var tree1 = (0, smart_1.vtree)({
            tag: 'ellipse',
            ns: 'svg',
            props: {
                href: 'https://google.com',
            },
        });
        (0, dom_1.diff)(null, tree1, document.body);
        (0, bun_test_1.expect)(document.body.children[0].getAttributeNS('http://www.w3.org/1999/xlink', 'href')).toBe('https://google.com');
        var tree2 = (0, smart_1.vtree)({
            tag: 'ellipse',
            ns: 'svg',
            props: {
                href: 'https://yahoo.com',
            },
        });
        (0, dom_1.diff)(tree1, tree2, document.body);
        (0, bun_test_1.expect)(document.body.children[0].getAttributeNS('http://www.w3.org/1999/xlink', 'href')).toBe('https://yahoo.com');
    });
    (0, bun_test_1.test)('Should create an SVG DOM node, with regular attribute', function () {
        var tree = (0, smart_1.vtree)({
            tag: 'ellipse',
            ns: 'svg',
            props: {
                rx: '100',
            },
        });
        (0, dom_1.diff)(null, tree, document.body);
        (0, bun_test_1.expect)(document.body.children[0].getAttribute('rx')).toBe('100');
    });
    (0, bun_test_1.test)('Should create an SVG DOM node, with regular attribute, and change it', function () {
        var tree1 = (0, smart_1.vtree)({
            tag: 'ellipse',
            ns: 'svg',
            props: {
                rx: '100',
            },
        });
        (0, dom_1.diff)(null, tree1, document.body);
        (0, bun_test_1.expect)(document.body.children[0].getAttribute('rx')).toBe('100');
        var tree2 = (0, smart_1.vtree)({
            tag: 'ellipse',
            ns: 'svg',
            props: {
                rx: '200',
            },
        });
        (0, dom_1.diff)(tree1, tree2, document.body);
        (0, bun_test_1.expect)(document.body.children[0].getAttribute('rx')).toBe('200');
    });
    (0, bun_test_1.test)('Should replace a Node with a new Node of a window different tag', function () {
        // populate DOM
        var tree1 = (0, smart_1.vtree)({ tag: 'div' });
        (0, dom_1.diff)(null, tree1, document.body);
        // Test node was populated
        (0, bun_test_1.expect)(document.body.children.length).toBe(1);
        // Replace node
        var tree2 = (0, smart_1.vtree)({ tag: 'a' });
        (0, dom_1.diff)(tree1, tree2, document.body);
        // Test node is removed from DOM
        (0, bun_test_1.expect)(document.body.children[0].tagName).toBe('A');
    });
    (0, bun_test_1.test)('Should create children', function () {
        // populate DOM
        var tree = (0, smart_1.vtree)({ children: [(0, smart_1.vtree)()] });
        (0, dom_1.diff)(null, tree, document.body);
        (0, bun_test_1.expect)(tree.domRef.children.length).toBe(1);
        (0, bun_test_1.expect)(tree.children.length).toBe(1);
    });
    (0, bun_test_1.test)('Should remove a child', function () {
        // populate DOM
        var tree1 = (0, smart_1.vtree)({ children: [(0, smart_1.vtree)()] });
        (0, dom_1.diff)(null, tree1, document.body);
        (0, bun_test_1.expect)(tree1.domRef.children.length).toBe(1);
        // remove children from DOM
        var tree2 = (0, smart_1.vtree)({ children: [] });
        (0, dom_1.diff)(tree1, tree2, document.body);
        (0, bun_test_1.expect)(tree2.domRef.childNodes.length).toBe(0);
    });
    (0, bun_test_1.test)('Should replace Node with TextNode', function () {
        var node = (0, smart_1.vtree)();
        (0, dom_1.diff)(null, node, document.body);
        (0, bun_test_1.expect)(document.body.childNodes.length).toBe(1);
        var textNode = (0, smart_1.vtree)({ type: 'vtext', 'text': 'fooo' });
        (0, dom_1.diff)(node, textNode, document.body);
        (0, bun_test_1.expect)(document.body.childNodes[0].textContent).toBe('fooo');
    });
    (0, bun_test_1.test)('Should replace TextNode with Node', function () {
        // populate DOM
        var textNode = (0, smart_1.vtext)('fooo');
        (0, dom_1.diff)(null, textNode, document.body);
        // Test node was populated
        (0, bun_test_1.expect)(document.body.childNodes.length).toBe(1);
        // Replace node
        var node = (0, smart_1.vtree)();
        (0, dom_1.diff)(textNode, node, document.body);
        // Test node is removed from DOM
        (0, bun_test_1.expect)(document.body.children[0].tagName).toBe('DIV');
    });
    (0, bun_test_1.test)('Should remove a DOM node', function () {
        // populate DOM
        var newNode = (0, smart_1.vtree)();
        (0, dom_1.diff)(null, newNode, document.body);
        // Test node was populated
        (0, bun_test_1.expect)(document.body.children.length).toBe(1);
        // Remove node
        (0, dom_1.diff)(newNode, null, document.body);
        // Test node is removed from DOM
        (0, bun_test_1.expect)(document.body.children.length).toBe(0);
    });
    (0, bun_test_1.test)('Should create a new property on a DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({
            props: { id: 'a' },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef['id']).toBe('a');
    });
    (0, bun_test_1.test)('Should skip if window diffing identical properties', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({
            props: { id: 'a' },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        var newNode = (0, smart_1.vtree)({
            props: { id: 'a' },
        });
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef).toBe(newNode.domRef);
    });
    (0, bun_test_1.test)('Should create a custom attribute on a DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({
            props: {
                lol: 'lol',
            },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef.getAttribute('lol')).toBe('lol');
    });
    (0, bun_test_1.test)('Should change a custom attribute on a DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({
            props: {
                lol: 'lol',
            },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef.getAttribute('lol')).toBe('lol');
        var newNode = (0, smart_1.vtree)({
            props: {
                lol: 'lolz',
            },
        });
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef.getAttribute('lol')).toBe('lolz');
    });
    (0, bun_test_1.test)('Should remove a custom attribute from a DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({
            props: {
                lol: 'lol',
            },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef.getAttribute('lol')).toBe('lol');
        // test property change
        var newNode = (0, smart_1.vtree)();
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef.getAttribute('lol')).toBe(null);
    });
    (0, bun_test_1.test)('Should remove a property from DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({ props: { id: 'someid' } });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef['id']).toBe('someid');
        // test property change
        var newNode = (0, smart_1.vtree)();
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef['id']).toBe('');
    });
    (0, bun_test_1.test)('Should change a property from DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({ props: { id: 'someid' } });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(currentNode.domRef['id']).toBe('someid');
        // test property change
        var newNode = (0, smart_1.vtree)({ props: { id: 'foo' } });
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef['id']).toBe('foo');
    });
    (0, bun_test_1.test)('Should create css on a DOM node', function () {
        // populate DOM
        var newNode = (0, smart_1.vtree)({
            css: {
                color: 'red',
            },
        });
        (0, dom_1.diff)(null, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef.style['color']).toBe('red');
    });
    (0, bun_test_1.test)('Should remove css from DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({
            css: {
                color: 'red',
            },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        // test css change
        var newNode = (0, smart_1.vtree)();
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef.style['color']).toBe('');
    });
    (0, bun_test_1.test)('Should change css on a DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({
            css: {
                color: 'red',
            },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        // test css change
        var newNode = (0, smart_1.vtree)({
            css: {
                color: 'blue',
            },
        });
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef.style['color']).toBe('blue');
    });
    (0, bun_test_1.test)('Should no-op change to css on a DOM node', function () {
        // populate DOM
        var currentNode = (0, smart_1.vtree)({
            css: {
                color: 'red',
            },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        // test css no-op change
        var newNode = (0, smart_1.vtree)({
            css: {
                color: 'red',
            },
        });
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.domRef.style['color']).toBe('red');
    });
    (0, bun_test_1.test)('Should call onCreated and onDestroyed', function () {
        // populate DOM
        var create = 0, destroy = 0;
        var currentNode = (0, smart_1.vtree)({
            onCreated: function () {
                create++;
            },
            onDestroyed: function () {
                destroy++;
            },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(create).toBe(1);
        (0, dom_1.diff)(currentNode, null, document.body);
        (0, bun_test_1.expect)(destroy).toBe(1);
    });
    (0, bun_test_1.test)('Should call onCreated and onBeforeDestroyed', function () {
        var create = 0, destroy = 0;
        var currentNode = (0, smart_1.vtree)({
            onCreated: function () {
                create++;
            },
            onBeforeDestroyed: function () {
                destroy++;
            },
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(create).toBe(1);
        (0, dom_1.diff)(currentNode, null, document.body);
        (0, bun_test_1.expect)(destroy).toBe(1);
    });
    (0, bun_test_1.test)('Should call onDestroyed recursively', function () {
        var destroy = 0, childDestroy = 0;
        var child = (0, smart_1.vtree)({
            onDestroyed: function () {
                childDestroy++;
            },
        });
        var parent = (0, smart_1.vtree)({
            onDestroyed: function () {
                destroy++;
            },
            children: [child],
        });
        (0, dom_1.diff)(null, parent, document.body);
        (0, dom_1.diff)(parent, null, document.body);
        (0, bun_test_1.expect)(destroy).toBe(1);
        (0, bun_test_1.expect)(childDestroy).toBe(1);
    });
    (0, bun_test_1.test)('Should call onBeforeDestroyed recursively', function () {
        var destroy = 0;
        var childDestroy = 0;
        var child = (0, smart_1.vtree)({
            onDestroyed: function () {
                childDestroy++;
            }
        });
        var parent = (0, smart_1.vtree)({
            onBeforeDestroyed: function () {
                destroy++;
            },
            children: [child],
        });
        (0, dom_1.diff)(null, parent, document.body);
        (0, dom_1.diff)(parent, null, document.body);
        (0, bun_test_1.expect)(destroy).toBe(1);
        (0, bun_test_1.expect)(childDestroy).toBe(1);
    });
    (0, bun_test_1.test)('Should recreate a DOM node when tags are the same but keys are different', function () {
        var destroy = 0;
        var currentNode = (0, smart_1.vtree)({
            onDestroyed: function () {
                destroy++;
            },
            key: 'key-1'
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        var newNode = (0, smart_1.vtree)({
            onDestroyed: function () {
                destroy++;
            },
            key: 'key-1'
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        (0, bun_test_1.expect)(destroy).toBe(0);
        (0, dom_1.diff)(currentNode, newNode, document.body);
        var newKeyedNode = (0, smart_1.vtree)({
            onDestroyed: function () {
                destroy++;
            },
            key: 'key-2'
        });
        (0, dom_1.diff)(currentNode, newKeyedNode, document.body);
        (0, bun_test_1.expect)(destroy).toBe(1);
    });
    (0, bun_test_1.test)('Should execute left-hand side happy path key-window diffing case', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vtree)({
            children: [
                (0, smart_1.vnodeKeyed)('div', 'a'),
                (0, smart_1.vnodeKeyed)('div', 'b'),
                (0, smart_1.vnodeKeyed)('div', 'c'),
            ],
            key: 'key-1',
        });
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vtree)({
            children: [
                (0, smart_1.vnodeKeyed)('div', 'a'),
                (0, smart_1.vnodeKeyed)('div', 'b'),
                (0, smart_1.vnodeKeyed)('div', 'c'),
            ],
            key: 'key-1',
        });
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(3);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(newNode.children[i].key).toBe(currentNode.children[i].key);
            (0, bun_test_1.expect)(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should diff keys properly when keys are prepended', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', '1')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', '2'), (0, smart_1.vnodeKeyed)('div', '1')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(2);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should execute right-hand side happy path key-window diffing case', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'a'), (0, smart_1.vnodeKeyed)('div', 'c')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'z'), (0, smart_1.vnodeKeyed)('div', 'c')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(2);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should swap nodes', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'a'), (0, smart_1.vnodeKeyed)('div', 'b')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'b'), (0, smart_1.vnodeKeyed)('div', 'a')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(2);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should execute flip-flop case', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'a'), (0, smart_1.vnodeKeyed)('div', 'b'), (0, smart_1.vnodeKeyed)('div', 'c')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'c'), (0, smart_1.vnodeKeyed)('div', 'b'), (0, smart_1.vnodeKeyed)('div', 'a')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(3);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
        for (var i = 0; i < 3; i++) {
            (0, bun_test_1.expect)(currentNode.children[i]).not.toBe(undefined);
            (0, bun_test_1.expect)(newNode.children[i]).not.toBe(undefined);
        }
    });
    (0, bun_test_1.test)('Should execute swapped case on 1k nodes', function () {
        var body = document.body;
        var kids = [];
        for (var i = 1; i < 1001; i++)
            kids.push((0, smart_1.vnodeKeyed)('div', i));
        var currentNode = (0, smart_1.vnode)('div', kids, {}, {}, 'html', null, null, null, null, 'key-1');
        var newKids = [];
        for (i = 1; i < 1001; i++) {
            if (i == 3) {
                newKids.push((0, smart_1.vnodeKeyed)('div', 999));
            }
            else if (i == 999) {
                newKids.push((0, smart_1.vnodeKeyed)('div', 3));
            }
            else {
                newKids.push((0, smart_1.vnodeKeyed)('div', i));
            }
        }
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', newKids, {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(1000);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
        for (i = 0; i < 1000; i++) {
            (0, bun_test_1.expect)(newNode.children[i].key).toBe(currentNode.children[i].key);
            (0, bun_test_1.expect)(newNode.children[i].children[0].text).toBe(currentNode.children[i].children[0].text);
            (0, bun_test_1.expect)(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
            (0, bun_test_1.expect)(newNode.children[i].domRef).not.toBe(undefined);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).not.toBe(undefined);
        }
    });
    (0, bun_test_1.test)('Should execute top-left and bottom-right match case', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [
            (0, smart_1.vnodeKeyed)('div', 'd'),
            (0, smart_1.vnodeKeyed)('div', 'a'),
            (0, smart_1.vnodeKeyed)('div', 'k'),
            (0, smart_1.vnodeKeyed)('div', 'r'),
            (0, smart_1.vnodeKeyed)('div', 'b'),
        ], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [
            (0, smart_1.vnodeKeyed)('div', 'a'),
            (0, smart_1.vnodeKeyed)('div', 'b'),
            (0, smart_1.vnodeKeyed)('div', 'r'),
            (0, smart_1.vnodeKeyed)('div', 'k'),
            (0, smart_1.vnodeKeyed)('div', 'd'),
        ], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(5);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should handle duplicate keys case', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [
            (0, smart_1.vnodeKeyed)('div', 'a'),
            (0, smart_1.vnodeKeyed)('div', 'a'),
            (0, smart_1.vnodeKeyed)('div', 'a'),
            (0, smart_1.vnodeKeyed)('div', 'b'),
            (0, smart_1.vnodeKeyed)('div', 'b'),
        ], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [
            (0, smart_1.vnodeKeyed)('div', 'b'),
            (0, smart_1.vnodeKeyed)('div', 'b'),
            (0, smart_1.vnodeKeyed)('div', 'b'),
            (0, smart_1.vnodeKeyed)('div', 'a'),
            (0, smart_1.vnodeKeyed)('div', 'a'),
        ], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(5);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should execute top-right and bottom-left match case', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [
            (0, smart_1.vnodeKeyed)('div', 'd'),
            (0, smart_1.vnodeKeyed)('div', 'a'),
            (0, smart_1.vnodeKeyed)('div', 'g'),
            (0, smart_1.vnodeKeyed)('div', 'b'),
        ], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [
            (0, smart_1.vnodeKeyed)('div', 'b'),
            (0, smart_1.vnodeKeyed)('div', 'g'),
            (0, smart_1.vnodeKeyed)('div', 'd'),
            (0, smart_1.vnodeKeyed)('div', 'a'),
        ], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(4);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should match nothing', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'e'), (0, smart_1.vnodeKeyed)('div', 'k'), (0, smart_1.vnodeKeyed)('div', 'l')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'b'), (0, smart_1.vnodeKeyed)('div', 'z'), (0, smart_1.vnodeKeyed)('div', 'j')], {}, {}, 'html', null, null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(3);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should handle nothing matches case where new key is found in old map', function () {
        var body = document.body;
        var currentNode = (0, smart_1.vnode)('div', [
            (0, smart_1.vnodeKeyed)('div', 'a'),
            (0, smart_1.vnodeKeyed)('div', 'k'),
            (0, smart_1.vnodeKeyed)('div', 'l'),
            (0, smart_1.vnodeKeyed)('div', 'c'),
            (0, smart_1.vnodeKeyed)('div', 'g'),
        ], {}, {}, 'html', null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, body);
        var newNode = (0, smart_1.vnode)('div', [
            (0, smart_1.vnodeKeyed)('div', 'b'),
            (0, smart_1.vnodeKeyed)('div', 'c'),
            (0, smart_1.vnodeKeyed)('div', 'l'),
            (0, smart_1.vnodeKeyed)('div', 'r'),
            (0, smart_1.vnodeKeyed)('div', 'k'),
        ], {}, {}, 'html', null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(5);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(currentNode.children[i].key).toEqual(newNode.children[i].key);
            (0, bun_test_1.expect)(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
        }
        (0, bun_test_1.expect)(currentNode.domRef.children).toEqual(newNode.domRef.children);
        (0, bun_test_1.expect)(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    });
    (0, bun_test_1.test)('Should append new nodes in keys patch', function () {
        var currentNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'a')], {}, {}, 'html', null, null, null, 'key-1');
        (0, dom_1.diff)(null, currentNode, document.body);
        var newNode = (0, smart_1.vnode)('div', [(0, smart_1.vnodeKeyed)('div', 'a'), (0, smart_1.vnodeKeyed)('div', 'c'), (0, smart_1.vnodeKeyed)('div', 'k')], {}, {}, 'html', null, null, null, 'key-1');
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(3);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(newNode.children[i].key).toBe(currentNode.children[i].key);
            (0, bun_test_1.expect)(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
        }
    });
    (0, bun_test_1.test)('Should diff keyed text nodes', function () {
        var currentNode = (0, smart_1.vtree)({
            tag: 'div',
            children: [
                (0, smart_1.vtree)({ type: 'vtext', text: 'foo', key: '1' }),
                (0, smart_1.vtree)({ type: 'vtext', text: 'bar', key: '2' }),
                (0, smart_1.vtree)({ type: 'vtext', text: 'baz', key: '3' })
            ]
        });
        (0, dom_1.diff)(null, currentNode, document.body);
        var newNode = (0, smart_1.vtree)({
            tag: 'div',
            children: [
                (0, smart_1.vtree)({ type: 'vtext', text: 'baz', key: '3' }),
                (0, smart_1.vtree)({ type: 'vtext', text: 'bar', key: '2' }),
                (0, smart_1.vtree)({ type: 'vtext', text: 'foo', key: '1' })
            ]
        });
        (0, dom_1.diff)(currentNode, newNode, document.body);
        (0, bun_test_1.expect)(newNode.children.length).toBe(currentNode.children.length);
        for (var i = 0; i < newNode.children.length; i++) {
            (0, bun_test_1.expect)(newNode.children[i].key).toBe(currentNode.children[i].key);
            (0, bun_test_1.expect)(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
        }
    });
});
