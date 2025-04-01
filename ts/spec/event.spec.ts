import { diff } from '../miso/dom';
import { delegate, undelegate, eventJSON } from '../miso/event';
import { EventCapture } from '../miso/types';
import { vtree } from '../miso/smart';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';

/* silence */
beforeAll(() => {
  console.log = () => {};
  console.info = () => {};
  console.warn = () => {};
  console.error = () => {};
});

/* reset DOM */
afterEach(() => {
  document.body.innerHTML = '';
});

describe ('Event tests', () => {

  test('Should delegate and undelegate button click', () => {
    var body = document.body;
    var count = 0;
    var result = null;
    var events = {
      click: {
        runEvent: (e) => {
          result = eventJSON('', e);
          count++;
        },
        options: {
          preventDefault: true,
          stopPropagation: false,
        },
      },
    };
    var vtreeChild = vtree({
      tag: 'button',
      events: events,
    });

    var vtreeParent = vtree({
      children: [vtreeChild],
      events: events,
    });

    /* initial page draw */
    diff(null, vtreeParent, document.body);

    /* ensure structures match */
    expect(vtreeParent.domRef).toEqual(document.body.childNodes[0]);
    expect(vtreeChild.domRef).toEqual(
      document.body.childNodes[0].childNodes[0],
    );

    /* setup event delegation */
    var getVTree = function (cb) {
      cb(vtreeParent);
    };
    const delegatedEvents : Array<EventCapture> = [{ name: 'click', capture: true }];
    delegate(body, delegatedEvents, getVTree, true);

    /* initiate click event */
    vtreeChild.domRef.click();

    /* check results */
    expect(count).toEqual(2);
    expect(result).not.toEqual(null);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true);
  });

});
