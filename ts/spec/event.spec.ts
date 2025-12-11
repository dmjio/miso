import { diff } from '../miso/dom';
import { delegate, undelegate } from '../miso/event';
import { DOMRef, EventCapture, VNode } from '../miso/types';
import { vnode, vcomp } from '../miso/smart';
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
    var events = {
        captures : {},
        bubbles : {
          click: {
            runEvent: (e : Event, o: Node) => {
              count++;
            },
            options: {
              preventDefault: true,
              stopPropagation: false,
            },
          },
        }
    };
    var vtreeChild = vnode({
      tag: 'button',
      events: events,
    });

    var vtreeParent = vnode({
      children: [vtreeChild],
      events: events,
    });

    vtreeChild.parent = vtreeParent;

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

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });


  test('Should propagate an event between components', () => {
    var body = document.body;
    var count = 0;
    var events = {
        captures : {},
        bubbles: {
          click: {
            runEvent: (e : Event, o: Node) => {
              count++;
            },
            options: {
              preventDefault: true,
              stopPropagation: false,
            },
          },
        }
    };
    var child = vnode({
      children: [ vnode({ tag: 'button' }) ],
      events
    });

    var childVComp = vcomp({
      eventPropagation: true,
      mount : function (p, f) {
        diff(null, child, p, drawingContext);
        return f(0, child);
      }
    });

    var parent = vnode({
      children: [childVComp],
      events: events,
    });

    var parentVComp = vcomp({
      mount : function (p, f) {
        diff(null, parent, p, drawingContext);
        return f(1, parent);
      }
    });

    /* create hierarchy */
    child.parent = childVComp;
    childVComp.parent = parent;
    parent.parent = parentVComp;

    /* initial page draw */
    diff(null, parentVComp, document.body, drawingContext);

    /* setup event delegation */
    var getVTree = (cb : any) => {
      cb(parentVComp);
    };
    const delegatedEvents : Array<EventCapture> = [{ name: 'click', capture: true }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    /* initiate click event */
    (child.domRef as HTMLElement).click();

    /* check results */
    expect(count).toEqual(2);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });

  test('Should *not* propagate an event between components', () => {
    var body = document.body;
    var count = 0;
    var events = {
      captures : {},
      bubbles: {
        click: {
          runEvent: (e : Event, o: Node) => {
            count++;
          },
          options: {
            preventDefault: true,
            stopPropagation: false,
          },
        }
      }
    };
    var child = vnode({
      children: [ vnode({ tag: 'button' }) ],
      events
    });

    var childVComp = vcomp({
      eventPropagation: false,
      mount : function (p, f) {
        diff(null, child, p, drawingContext);
        return f(0, child);
      }
    });

    var parent = vnode({
      children: [childVComp],
      events: events,
    });

    var parentVComp = vcomp({
      mount : function (p, f) {
        diff(null, parent, p, drawingContext);
        return f(1, parent);
      }
    });

    /* create hierarchy */
    child.parent = childVComp;
    childVComp.parent = parent;
    parent.parent = parentVComp;

    /* initial page draw */
    diff(null, parentVComp, document.body, drawingContext);

    /* setup event delegation */
    var getVTree = (cb : any) => {
      cb(parentVComp);
    };
    const delegatedEvents : Array<EventCapture> = [{ name: 'click', capture: true }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    /* initiate click event */
    (child.domRef as HTMLElement).click();

    /* check results */
    expect(count).toEqual(1);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });

  test('Should *not* propagate an event when stopPropagation is set', () => {
    var body = document.body;
    var count = 0;
    var events = {
      captures : {},
      bubbles: {
        click: {
          runEvent: (e : Event, o: Node) => {
            count++;
          },
          options: {
            preventDefault: true,
            stopPropagation: true,
          },
        },
      }
    };
    var child = vnode({
      children: [ vnode({ tag: 'button' }) ],
      events
    });

    var childVComp = vcomp({
      eventPropagation: true,
      mount : function (p: DOMRef, f) {
        diff(null, child, p, drawingContext);
        return f(0, child as VNode<DOMRef>);
      }
    });

    var parent = vnode({
      children: [childVComp],
      events: events,
    });

    var parentVComp = vcomp({
      mount : function (p: DOMRef, f) {
        diff(null, parent, p, drawingContext);
        return f(1, parent as VNode<DOMRef>);
      }
    });

    /* create hierarchy */
    child.parent = childVComp;
    childVComp.parent = parent as VNode<DOMRef>;
    parent.parent = parentVComp;

    /* initial page draw */
    diff(null, parentVComp, document.body, drawingContext);

    /* setup event delegation */
    var getVTree = (cb : any) => {
      cb(parentVComp);
    };
    const delegatedEvents : Array<EventCapture> = [{ name: 'click', capture: true }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    /* initiate click event */
    (child.domRef as HTMLElement).click();

    /* check results */
    expect(count).toEqual(1);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });

  test('Should call capture, bubble and target handlers in order', () => {
    var body = document.body;
    var counts = [];
    var events = {
      captures: {
        click: {
          runEvent: (e : Event, o: Node) => {
            counts.push(1);
          },
          options: {
            preventDefault: false,
            stopPropagation: false,
          },
        },
      },
      bubbles: {
        click: {
          runEvent: (e : Event, o: Node) => {
            counts.push(2);
          },
          options: {
            preventDefault: false,
            stopPropagation: false,
          },
        },
      }
    };
    var child = vnode({
      tag: 'button',
      events
    });

    var parent = vnode({
      children: [child],
      events,
    });

    /* create hierarchy */
    child.parent = parent;

    /* initial page draw */
    diff(null, parent, document.body, drawingContext);

    /* setup event delegation */
    var getVTree = (cb : any) => {
      cb(parent);
    };
    const delegatedEvents : Array<EventCapture> = [{ name: 'click', capture: false }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    /* initiate click event */
    (child.domRef as HTMLElement).click();

    /* check results */
    expect(counts).toEqual([1,1,2,2]);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });


  test('If stopPropagation called during capture phase, no target nor bubble phase should occur', () => {
    var body = document.body;
    var counts = [];
    var events = {
      captures: {
        click: {
          runEvent: (e : Event, o: Node) => {
            counts.push(1);
          },
          options: {
            preventDefault: false,
            stopPropagation: true,
          },
        },
      },
      bubbles: {
        click: {
          runEvent: (e : Event, o: Node) => {
            counts.push(2);
          },
          options: {
            preventDefault: false,
            stopPropagation: false,
          },
        },
      }
    };
    var child = vnode({
      tag: 'button',
      events
    });

    var parent = vnode({
      children: [child],
      events,
    });

    /* create hierarchy */
    child.parent = parent;

    /* initial page draw */
    diff(null, parent, document.body, drawingContext);

    /* setup event delegation */
    var getVTree = (cb : any) => {
      cb(parent);
    };
    const delegatedEvents : Array<EventCapture> = [{ name: 'click', capture: false }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    /* initiate click event */
    (child.domRef as HTMLElement).click();

    /* check results */
    expect(counts).toEqual([1]);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });

  test('If stopPropagation called during bubble phase, capture and target still get executed', () => {
    var body = document.body;
    var counts = [];
    var events = {
      captures: {
        click: {
          runEvent: (e : Event, o: Node) => {
            counts.push(1);
          },
          options: {
            preventDefault: false,
            stopPropagation: false,
          },
        },
      },
      bubbles: {
        click: {
          runEvent: (e : Event, o: Node) => {
            counts.push(2);
          },
          options: {
            preventDefault: false,
            stopPropagation: true,
          },
        },
      }
    };
    var child = vnode({
      tag: 'button',
      events
    });

    var parent = vnode({
      children: [child],
      events,
    });

    /* create hierarchy */
    child.parent = parent;

    /* initial page draw */
    diff(null, parent, document.body, drawingContext);

    /* setup event delegation */
    var getVTree = (cb : any) => {
      cb(parent);
    };
    const delegatedEvents : Array<EventCapture> = [{ name: 'click', capture: false }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    /* initiate click event */
    (child.domRef as HTMLElement).click();

    /* check results */
    expect(counts).toEqual([1,1,2]);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });

});
