"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var dom_1 = require("../miso/dom");
var event_1 = require("../miso/event");
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
(0, bun_test_1.describe)('Event tests', function () {
    (0, bun_test_1.test)('Should delegate and undelegate button click', function () {
        var body = document.body;
        var count = 0;
        var result = null;
        var events = {
            click: {
                runEvent: function (e) {
                    result = (0, event_1.eventJSON)([[]], e);
                    count++;
                },
                options: {
                    preventDefault: true,
                    stopPropagation: false,
                },
            },
        };
        var vtreeChild = (0, smart_1.vtree)({
            tag: 'button',
            events: events,
        });
        var vtreeParent = (0, smart_1.vtree)({
            children: [vtreeChild],
            events: events,
        });
        /* initial page draw */
        (0, dom_1.diff)(null, vtreeParent, document.body);
        /* ensure structures match */
        (0, bun_test_1.expect)(vtreeParent.domRef).toEqual(document.body.childNodes[0]);
        (0, bun_test_1.expect)(vtreeChild.domRef).toEqual(document.body.childNodes[0].childNodes[0]);
        /* setup event delegation */
        var getVTree = function (cb) {
            cb(vtreeParent);
        };
        events = [{ name: 'click', capture: true }];
        (0, event_1.delegate)(body, events, getVTree, true);
        /* initiate click event */
        vtreeChild.domRef.click();
        /* check results */
        (0, bun_test_1.expect)(count).toEqual(2);
        (0, bun_test_1.expect)(result).not.toEqual(null);
        /* unmount delegation */
        (0, event_1.undelegate)(document.body, events, getVTree, true);
    });
});
