"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/* imports */
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
/* tests */
(0, bun_test_1.describe)('Event tests', function () {
    (0, bun_test_1.test)('Should unmount recursively in order', function () {
        var unmounts = [];
        var mkVComp = function (name, children) {
            return (0, smart_1.vtree)({
                type: 'vcomp',
                children: children,
                'data-component-id': name,
                unmount: function () {
                    unmounts.push(name);
                }
            });
        };
        var tree = mkVComp('one', [mkVComp('two', [mkVComp('three', [])])]);
        (0, dom_1.diff)(null, tree, document.body);
        (0, dom_1.diff)(tree, null, document.body);
        (0, bun_test_1.expect)(unmounts).toEqual(['one', 'two', 'three']);
    });
    (0, bun_test_1.test)('Should detect duplicate component mounting', function () {
        var mountCount = 0;
        var newComp1 = (0, smart_1.vtree)({
            type: 'vcomp',
            mount: function () {
                mountCount++;
            },
            'data-component-id': 'vcomp-foo',
        });
        (0, dom_1.diff)(null, newComp1, document.body);
        var newComp2 = (0, smart_1.vtree)({
            type: 'vcomp',
            mount: function () {
                mountCount++;
            },
            'data-component-id': 'vcomp-foo',
        });
        var newNode = (0, smart_1.vtree)({ children: [newComp2] });
        (0, dom_1.diff)(null, newNode, document.body);
        (0, bun_test_1.expect)(mountCount).toBe(1);
    });
    (0, bun_test_1.test)('Should mount and unmount a component', function () {
        var mountCount = 0;
        var unmountCount = 0;
        var newNode = (0, smart_1.vtree)({
            type: 'vcomp',
            mount: function (cb) {
                mountCount++;
                var node = (0, smart_1.vtree)();
                (0, dom_1.diff)(null, node, document.body);
                cb(node);
            },
            unmount: function () {
                unmountCount++;
            },
            props: { id: 'vcomp-foo' },
            css: {
                'background-color': 'red',
            },
        });
        (0, dom_1.diff)(null, newNode, document.body);
        (0, bun_test_1.expect)(mountCount).toBe(1);
        (0, bun_test_1.expect)(newNode.children.length).toBe(1);
        (0, bun_test_1.expect)(newNode.domRef.children.length).toBe(1);
        (0, bun_test_1.expect)(newNode.domRef.id).toBe('vcomp-foo');
        (0, bun_test_1.expect)(newNode.domRef.style['background-color']).toBe('red');
        (0, dom_1.diff)(newNode, null, document.body);
        (0, bun_test_1.expect)(unmountCount).toBe(1);
    });
    (0, bun_test_1.test)('Should Diff attrs of two Components', function () {
        // populate DOM
        var mountCount = 0;
        var compNode1 = (0, smart_1.vtree)({
            type: 'vcomp',
            mount: function () {
                mountCount++;
            },
            'data-component-id': 'vcomp-foo',
            css: { 'background-color': 'red' },
        });
        (0, dom_1.diff)(null, compNode1, document.body);
        (0, bun_test_1.expect)(mountCount).toBe(1);
        // Test node was populated
        (0, bun_test_1.expect)(document.body.childNodes.length).toBe(1);
        (0, bun_test_1.expect)(document.body.childNodes[0].style['background-color']).toBe('red');
        // Replace node
        mountCount = 0;
        var compNode2 = (0, smart_1.vtree)({
            type: 'vcomp',
            mount: function () {
                mountCount++;
            },
            'data-component-id': 'vcomp-foo',
            css: { 'background-color': 'green' },
        });
        (0, dom_1.diff)(compNode1, compNode2, document.body);
        (0, bun_test_1.expect)(document.body.childNodes[0].style['background-color']).toBe('green');
    });
    (0, bun_test_1.test)('Should replace Node with Component', function () {
        // populate DOM
        var node = (0, smart_1.vtree)();
        (0, dom_1.diff)(null, node, document.body);
        // Test node was populated
        (0, bun_test_1.expect)(document.body.childNodes.length).toBe(1);
        // Replace node
        var mountCount = 0;
        var compNode = (0, smart_1.vtree)({
            type: 'vcomp',
            'data-component-id': 'vcomp-id',
            mount: function () {
                mountCount++;
            },
        });
        (0, dom_1.diff)(node, compNode, document.body);
        // Node is removed from DOM, Component is on the DOM
        (0, bun_test_1.expect)(document.body.childNodes[0].getAttribute('data-component-id')).toBe('vcomp-id');
        (0, bun_test_1.expect)(mountCount).toBe(1);
    });
    (0, bun_test_1.test)('Should replace Text with Component', function () {
        // populate DOM
        var node = (0, smart_1.vtree)({ type: 'vtext', 'text': 'foo' });
        (0, dom_1.diff)(null, node, document.body);
        // Test node was populated
        (0, bun_test_1.expect)(node.domRef.textContent).toBe('foo');
        (0, bun_test_1.expect)(document.body.childNodes.length).toBe(1);
        // Replace node
        var mountCount = 0;
        var compNode = (0, smart_1.vtree)({
            type: 'vcomp',
            'data-component-id': 'vcomp-id',
            mount: function () {
                mountCount++;
            },
        });
        (0, dom_1.diff)(node, compNode, document.body);
        // Node is removed from DOM, Component is on the DOM
        (0, bun_test_1.expect)(document.body.childNodes[0].getAttribute('data-component-id')).toBe('vcomp-id');
        (0, bun_test_1.expect)(mountCount).toBe(1);
    });
    (0, bun_test_1.test)('Should replace Component with TextNode', function () {
        var mountCount = 0, unmountCount = 0;
        var component = (0, smart_1.vtree)({
            mount: function () {
                return mountCount++;
            },
            unmount: function () {
                return unmountCount++;
            },
            type: 'vcomp',
        });
        (0, dom_1.diff)(null, component, document.body);
        // Test component was populated
        (0, bun_test_1.expect)(document.body.childNodes.length).toBe(1);
        (0, bun_test_1.expect)(mountCount).toBe(1);
        (0, bun_test_1.expect)(unmountCount).toBe(0);
        // Replace component
        var textNode = (0, smart_1.vtext)('fooo');
        (0, dom_1.diff)(component, textNode, document.body);
        // Test node is removed from DOM
        (0, bun_test_1.expect)(document.body.childNodes[0].textContent).toBe('fooo');
        (0, bun_test_1.expect)(unmountCount).toBe(1);
    });
    (0, bun_test_1.test)('Should replace Component with Node', function () {
        // populate DOM
        var mountCount = 0, unmountCount = 0;
        var component = (0, smart_1.vtree)({
            type: 'vcomp',
            mount: function () {
                mountCount++;
            },
            unmount: function () {
                unmountCount++;
            },
        });
        (0, dom_1.diff)(null, component, document.body);
        // Test component was populated
        (0, bun_test_1.expect)(document.childNodes.length).toBe(1);
        (0, bun_test_1.expect)(mountCount).toBe(1);
        (0, bun_test_1.expect)(unmountCount).toBe(0);
        // Replace component
        (0, dom_1.diff)(component, (0, smart_1.vtree)(), document.body);
        // Test node is removed from DOM
        (0, bun_test_1.expect)(document.body.children[0].tagName).toBe('DIV');
        (0, bun_test_1.expect)(unmountCount).toBe(1);
    });
});
