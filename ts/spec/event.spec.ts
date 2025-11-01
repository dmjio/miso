import { diff } from '../miso/dom';
import { delegate, undelegate, eventJSON } from '../miso/event';
import { DOMRef, EventCapture } from '../miso/types';
import { vnode } from '../miso/smart';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { eventContext, drawingContext } from '../miso/context/dom';

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
        runEvent: (e : Event, o: Node) => {
          result = eventJSON('', e);
          count++;
        },
        options: {
          preventDefault: true,
          stopPropagation: false,
        },
      },
    };
    var vtreeChild = vnode({
      tag: 'button',
      events: events,
    });

    var vtreeParent = vnode({
      children: [vtreeChild],
      events: events,
    });

    /* initial page draw */
    diff(null, vtreeParent, document.body, drawingContext);

    /* ensure structures match */
    expect(vtreeParent.domRef).toEqual(document.body.childNodes[0] as DOMRef);
    expect(vtreeChild.domRef).toEqual(
        document.body.childNodes[0].childNodes[0] as DOMRef
    );

    /* setup event delegation */
    var getVTree = (cb : any) => {
      cb(vtreeParent);
    };
    const delegatedEvents : Array<EventCapture> = [{ name: 'click', capture: true }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    /* initiate click event */
   (vtreeChild.domRef as HTMLElement).click();

    /* check results */
    expect(count).toEqual(2);
    expect(result).not.toEqual(null);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });

});
