"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
/* imports */
var iso_1 = require("../miso/iso");
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
(0, bun_test_1.describe)("Hydration tests", function () {
    (0, bun_test_1.test)('Should copy simple nested DOM into VTree', function () {
        var div = document.createElement('div');
        document.body.appendChild(div);
        var nestedDiv = document.createElement('div');
        div.appendChild(nestedDiv);
        var txt = document.createTextNode('foo');
        nestedDiv.appendChild(txt);
        var currentNode = (0, smart_1.vtree)({
            children: [(0, smart_1.vtree)({ children: [(0, smart_1.vtext)('foo')] })],
        });
        (0, iso_1.hydrate)(false, document.body, currentNode);
        (0, bun_test_1.expect)(currentNode.children[0].children[0].text).toEqual('foo');
    });
    (0, bun_test_1.test)('Should fail because of expecting text node', function () {
        var div = document.createElement('div');
        document.body.appendChild(div);
        var nestedDiv = document.createElement('div');
        div.appendChild(nestedDiv);
        var currentNode = (0, smart_1.vtree)({ children: [(0, smart_1.vtext)('foo')] });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, currentNode)).toEqual(false);
    });
    (0, bun_test_1.test)('Should fail to hydrate because of expecting element', function () {
        var div = document.createElement('div');
        document.body.appendChild(div);
        var txt = document.createTextNode('foo');
        div.appendChild(txt);
        var currentNode = (0, smart_1.vtree)({
            children: [(0, smart_1.vtree)()],
        });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, currentNode)).toEqual(false);
    });
    (0, bun_test_1.test)('Should fail to hydrate because of non-matching text', function () {
        var div = document.createElement('div');
        document.body.appendChild(div);
        var txt = document.createTextNode('foo');
        div.appendChild(txt);
        var currentNode = (0, smart_1.vtree)({ children: [(0, smart_1.vtext)('bar')] });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, currentNode)).toEqual(false);
    });
    (0, bun_test_1.test)('Should fail to hydrate because of non-matching DOM and VDOM', function () {
        var div = document.createElement('div');
        document.body.appendChild(div);
        var txt = document.createTextNode('foobar');
        div.appendChild(txt);
        var currentNode = (0, smart_1.vtree)({ children: [(0, smart_1.vtext)('foo')] });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, currentNode)).toEqual(false);
    });
    (0, bun_test_1.test)('Should copy DOM into VTree with multiple consecutive text nodes and collapse them', function () {
        var div = document.createElement('div');
        document.body.appendChild(div);
        var txt = document.createTextNode('foobarbaz');
        div.appendChild(txt);
        var currentNode = (0, smart_1.vtree)({
            children: [(0, smart_1.vtext)('foo'), (0, smart_1.vtext)('bar'), (0, smart_1.vtext)('baz')],
        });
        (0, iso_1.hydrate)(false, document.body, currentNode);
        (0, bun_test_1.expect)(div.childNodes[0].textContent).toEqual('foobarbaz');
    });
    (0, bun_test_1.test)('Should copy DOM into VTree with multiple consecutive text nodes and collapse them without mount point', function () {
        var div = document.createElement('div');
        document.body.appendChild(div);
        var txt = document.createTextNode('foobarbaz');
        div.appendChild(txt);
        var currentNode = (0, smart_1.vtree)({
            children: [
                (0, smart_1.vtext)('foo'),
                (0, smart_1.vtext)('bar'),
                (0, smart_1.vtext)('baz'),
                (0, smart_1.vtree)(),
                (0, smart_1.vtext)('foo'),
                (0, smart_1.vtext)('bar'),
                (0, smart_1.vtext)('baz'),
            ],
        });
        (0, iso_1.hydrate)(false, null, currentNode);
        // Expect "foobarbaz" to be split up into three nodes in the DOM
        (0, bun_test_1.expect)(div.childNodes[0].textContent).toEqual('foobarbaz');
        (0, bun_test_1.expect)(div.childNodes[2].textContent).toEqual('foobarbaz');
    });
    (0, bun_test_1.test)('Should copy DOM into VTree at mountPoint', function () {
        var unrelatedDiv = document.createElement('div');
        document.body.appendChild(document.createElement('script'));
        document.body.appendChild(document.createTextNode('test'));
        document.body.appendChild(unrelatedDiv);
        var unrelatedTxt = document.createTextNode('Not part of Miso app');
        unrelatedDiv.appendChild(unrelatedTxt);
        var misoDiv = document.createElement('div');
        document.body.appendChild(misoDiv);
        var nestedDiv1 = document.createElement('div');
        misoDiv.appendChild(nestedDiv1);
        var nestedDiv2 = document.createElement('div');
        nestedDiv1.appendChild(nestedDiv2);
        var txt = document.createTextNode('foo');
        nestedDiv2.appendChild(txt);
        var tree = (0, smart_1.vtree)({ children: [(0, smart_1.vtree)({ children: [(0, smart_1.vtext)('foo')] })] });
        var succeeded = (0, iso_1.hydrate)(false, misoDiv, tree);
        (0, bun_test_1.expect)(tree.children[0].children[0].domRef).toEqual(txt);
        (0, bun_test_1.expect)(succeeded).toEqual(true);
    });
    (0, bun_test_1.test)('Should copy DOM into VTree at body w/ script / text siblings', function () {
        var unrelatedDiv = document.createElement('div');
        document.body.appendChild(document.createElement('script'));
        document.body.appendChild(document.createTextNode('test'));
        document.body.appendChild(unrelatedDiv);
        var unrelatedTxt = document.createTextNode('Not part of Miso app');
        unrelatedDiv.appendChild(unrelatedTxt);
        var misoDiv = document.createElement('div');
        document.body.appendChild(misoDiv);
        var nestedDiv1 = document.createElement('div');
        misoDiv.appendChild(nestedDiv1);
        var nestedDiv2 = document.createElement('div');
        nestedDiv1.appendChild(nestedDiv2);
        var txt = document.createTextNode('foo');
        nestedDiv2.appendChild(txt);
        var currentNode = (0, smart_1.vnodeKids)('div', [(0, smart_1.vnodeKids)('div', [(0, smart_1.vtext)('foo')])]);
        var succeeded = (0, iso_1.hydrate)(true, document.body, currentNode);
        (0, bun_test_1.expect)(currentNode.children[0].children[0].domRef.textContent).toEqual('foo');
        (0, bun_test_1.expect)(succeeded).toEqual(false);
    });
    (0, bun_test_1.test)('Should fail to mount on a text node', function () {
        var misoTxt = document.createTextNode('foo');
        document.body.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({ children: [(0, smart_1.vtree)({ children: [(0, smart_1.vtext)('foo')] })] });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(true, misoTxt, tree)).toEqual(false);
    });
    (0, bun_test_1.test)('Should not hydrate on an empty page', function () {
        var tree = (0, smart_1.vtree)({ children: [(0, smart_1.vtree)({ children: [(0, smart_1.vtext)('foo')] })] });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(true, null, tree)).toEqual(false);
    });
    (0, bun_test_1.test)('Should pass integrity check', function () {
        var body = document.body;
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({ children: [(0, smart_1.vtext)('foo')] });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
    });
    (0, bun_test_1.test)('Should fail integrity check on bad tag', function () {
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({ children: [(0, smart_1.vtext)('foo')] });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.tag = 'lol';
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on bad tag in hydrate w/ logging enabled', function () {
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({ children: [(0, smart_1.vtext)('fool')] });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(true, document.body, tree)).toEqual(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on differing vtext', function () {
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            children: [(0, smart_1.vtext)('foo')],
        });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.children[0].text = 'oops';
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on differing child lengths', function () {
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            children: [(0, smart_1.vtext)('foo')],
        });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.children = [];
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on differing styles', function () {
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        child.style['background-color'] = 'red';
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            children: [{ type: 'vtext', text: 'foo' }],
            css: { 'background-color': 'red' },
        });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.css['background-color'] = 'green';
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on differing styles, for color', function () {
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        child.style['background-color'] = 'red';
        child.style['color'] = '#cccccc';
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            children: [{ type: 'vtext', text: 'foo' }],
            css: { 'background-color': 'red', color: '#cccccc' },
        });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.css['color'] = '#dddddd';
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on differing props', function () {
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        child.style['background-color'] = 'red';
        child.className = 'something';
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            props: { class: 'something' },
            children: [{ type: 'vtext', text: 'foo' }],
            css: { 'background-color': 'red' },
        });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.props['class'] = 'something-else';
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on differing height / width', function () {
        var child = document.createElement('img');
        var misoTxt = document.createTextNode('foo');
        child.style['background-color'] = 'red';
        child.className = 'something';
        child.height = 100;
        child.width = 100;
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            tag: 'img',
            props: { class: 'something', height: '100', width: '100' },
            children: [{ type: 'vtext', text: 'foo' }],
            css: { 'background-color': 'red' },
        });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.props['height'] = '200';
        tree.props['width'] = '200';
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on random property (title)', function () {
        var child = document.createElement('div');
        var misoTxt = document.createTextNode('foo');
        child['title'] = 'bar';
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            props: { title: 'bar' },
            children: [(0, smart_1.vtext)('foo')],
        });
        (0, bun_test_1.expect)((0, iso_1.hydrate)(false, document.body, tree)).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.props['title'] = 'woz';
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on href', function () {
        var child = document.createElement('a');
        var misoTxt = document.createTextNode('foo');
        child.style['background-color'] = 'red';
        child.href = 'google.com';
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            tag: 'a',
            props: { href: 'google.com' },
            children: [{ type: 'vtext', text: 'foo' }],
            css: { 'background-color': 'red' },
        });
        var result = (0, iso_1.hydrate)(false, document.body, tree);
        (0, bun_test_1.expect)(result).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.props['href'] = 'notgoogle.com';
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on vtext domRef', function () {
        var child = document.createElement('a');
        var misoTxt = document.createTextNode('foo');
        child.style['background-color'] = 'red';
        child.href = 'google.com';
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            tag: 'a',
            props: { href: 'google.com' },
            children: [(0, smart_1.vtext)('foo')],
            css: { 'background-color': 'red' },
        });
        var result = (0, iso_1.hydrate)(false, document.body, tree);
        (0, bun_test_1.expect)(result).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(true);
        tree.children[0].domRef = document.createElement('div');
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
    (0, bun_test_1.test)('Should fail integrity check on unknown property test', function () {
        var child = document.createElement('a');
        var misoTxt = document.createTextNode('foo');
        document.body.appendChild(child);
        child.appendChild(misoTxt);
        var tree = (0, smart_1.vtree)({
            tag: 'a',
            props: { foobah: 'lol' },
            children: [(0, smart_1.vtext)('foo')],
            ns: 'HTML',
        });
        var result = (0, iso_1.hydrate)(false, document.body, tree);
        (0, bun_test_1.expect)(result).toEqual(true);
        (0, bun_test_1.expect)((0, iso_1.integrityCheck)(tree)).toBe(false);
    });
});
