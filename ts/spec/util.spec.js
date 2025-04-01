"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var util_1 = require("../miso/util");
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
(0, bun_test_1.describe)('Utils tests', function () {
    (0, bun_test_1.test)('Should set body[data-component-id] via setBodyComponent()', function () {
        (0, util_1.setBodyComponent)('component-one');
        (0, bun_test_1.expect)(document.body.getAttribute('data-component-id')).toEqual('component-one');
    });
    (0, bun_test_1.test)('Should call callFocus() and callBlur()', function () {
        var child = document.createElement('input');
        child['id'] = 'foo';
        document.body.appendChild(child);
        (0, util_1.callFocus)('blah', 0); /* missing case */
        (0, util_1.callFocus)('foo', 0); /* found case */
        (0, util_1.callFocus)('foo', 1); /* found case */
        (0, bun_test_1.expect)(document.activeElement).toEqual(child);
        (0, util_1.callBlur)('blah', 0); /* missing case */
        (0, util_1.callBlur)('foo', 0); /* found case */
        (0, util_1.callBlur)('foo', 1); /* found case */
        (0, bun_test_1.expect)(document.activeElement).toEqual(document.body);
    });
});
