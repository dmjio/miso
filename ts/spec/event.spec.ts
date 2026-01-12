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

  test('Should warn when clicking mount with no target handler (empty stack)', () => {
    const body = document.body;
    const parent = vnode({ tag: 'div', children: [vnode({ tag: 'span' })], events: { captures: {}, bubbles: {} } });
    (parent.children[0] as any).parent = parent;
    diff(null, parent, body, drawingContext);

    const getVTree = (cb: any) => cb(parent);
    const delegatedEvents: Array<EventCapture> = [{ name: 'click', capture: true }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    // Click on the mount itself to produce an empty stack in delegateEvent
    (body as HTMLElement).click();

    undelegate(body, delegatedEvents, getVTree, true, eventContext);
  });

  test('Should propagate to parent when target has no bubble handler', () => {
    const body = document.body;
    let count = 0;
    const childEvents = { captures: {}, bubbles: {} };
    const parentEvents = {
      captures: {},
      bubbles: {
        click: {
          runEvent: () => { count++; },
          options: { preventDefault: false, stopPropagation: false }
        }
      }
    };

    const child = vnode({ tag: 'button', events: childEvents });
    const parent = vnode({ children: [child], events: parentEvents });
    (child as any).parent = parent;
    diff(null, parent, body, drawingContext);

    const getVTree = (cb: any) => cb(parent);
    const delegatedEvents: Array<EventCapture> = [{ name: 'click', capture: false }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    (child.domRef as HTMLElement).click();

    expect(count).toBe(1);

    undelegate(body, delegatedEvents, getVTree, true, eventContext);
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
      mount: (p) => {
        diff(null, child, p, drawingContext);
        return { componentId: 0, componentTree: child };
      },
    });

    var parent = vnode({
      children: [childVComp],
      events: events,
    });

    var parentVComp = vcomp({
      mount: (p) => {
        diff(null, parent, p, drawingContext);
        return { componentId: 1, componentTree: parent };
      },
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
      mount: (p) => {
        diff(null, child, p, drawingContext);
        return { componentId: 0, componentTree: child };
      },
    });

    var parent = vnode({
      children: [childVComp],
      events: events,
    });

    var parentVComp = vcomp({
      mount: (p) => {
        diff(null, parent, p, drawingContext);
        return { componentId: 1, componentTree: parent };
      },
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
      mount: (p: DOMRef) => {
        diff(null, child, p, drawingContext);
        return { componentId: 0, componentTree: child as VNode<DOMRef> };
      },
    });

    var parent = vnode({
      children: [childVComp],
      events: events,
    });

    var parentVComp = vcomp({
      mount: (p: DOMRef) => {
        diff(null, parent, p, drawingContext);
        return { componentId: 1, componentTree: parent as VNode<DOMRef> };
      },
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

  test('Should delegate events through recursively mounted components (vcomp -> vcomp -> vnode)', () => {
    var body = document.body;
    var count = 0;
    var events = {
      captures: {},
      bubbles: {
        click: {
          runEvent: (e: Event, o: Node) => {
            count++;
          },
          options: {
            preventDefault: true,
            stopPropagation: false,
          },
        }
      }
    };

    // Innermost vnode with button that has click event
    var innerNode = vnode({
      tag: 'div',
      children: [vnode({ tag: 'button', events })],
      events
    });

    // Middle vcomp wrapping the vnode
    var middleVComp = vcomp({
      eventPropagation: true,
      mount: (p) => {
        diff(null, innerNode, p, drawingContext);
        return { componentId: 1, componentTree: innerNode };
      },
    });

    // Outer vcomp wrapping the middle vcomp
    var outerVComp = vcomp({
      eventPropagation: true,
      mount: (p) => {
        diff(null, middleVComp, p, drawingContext);
        return { componentId: 0, componentTree: middleVComp };
      },
    });

    // Set up parent hierarchy
    const buttonNode = innerNode.children[0] as VNode<DOMRef>;
    (buttonNode as any).parent = innerNode;
    (innerNode as any).parent = middleVComp;
    (middleVComp as any).parent = outerVComp;

    /* initial page draw */
    diff(null, outerVComp, document.body, drawingContext);

    /* verify DOM structure is correct - recursively mounted components create the inner div */
    expect(document.body.childNodes.length).toBeGreaterThanOrEqual(1);
    const divElement = Array.from(document.body.childNodes).find(
      node => (node as Element).tagName === 'DIV'
    ) as Element;
    expect(divElement).toBeDefined();
    expect(divElement.tagName).toBe('DIV');

    /* setup event delegation */
    var getVTree = (cb: any) => {
      cb(outerVComp);
    };
    const delegatedEvents: Array<EventCapture> = [{ name: 'click', capture: true }];
    delegate(body, delegatedEvents, getVTree, true, eventContext);

    /* initiate click event on the button inside nested components */
    const button = buttonNode.domRef as HTMLElement;
    button.click();

    /* check results - event should propagate through the component hierarchy */
    expect(count).toEqual(2);

    /* unmount delegation */
    undelegate(document.body, delegatedEvents, getVTree, true, eventContext);
  });

});
