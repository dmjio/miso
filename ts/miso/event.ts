import { VTree, EventCapture, EventObject, Options } from './types';

/* event delegation algorithm */
export function delegate(
  mount: HTMLElement,
  events: Array<EventCapture>,
  getVTree: ((vtree: VTree) => void),
  debug: boolean,
) {
  for (const event of events) {
    mount.addEventListener(
      event['name'],
      function (e: Event) {
        listener(e, mount, getVTree, debug);
      },
      event['capture'],
    );
  }
}
/* event undelegation */
export function undelegate (
  mount: HTMLElement,
  events: Array<EventCapture>,
  getVTree: ((vtree: VTree) => void),
  debug: boolean,
) {
  for (const event of events) {
    mount.removeEventListener(
      event['name'],
      function (e: Event) {
        listener(e, mount, getVTree, debug);
      },
      event['capture'],
    );
  }
}
/* the event listener shared by both delegator and undelegator */
function listener (
  e: Event
, mount: HTMLElement
, getVTree: (VTree) => void
, debug: boolean
) {
  getVTree(function (obj: VTree) {
    if (e.target) {
      delegateEvent(e, obj, buildTargetToElement(mount, e.target), [], debug);
    }
  });
};
/* Create a stack of ancestors used to index into the virtual DOM */
function buildTargetToElement (element: HTMLElement, target: any) {
  var stack = [];
  while (element !== target) {
    stack.unshift(target);
    target = target.parentNode;
  }
  return stack;
};
/* Finds event in virtual dom via pointer equality
       Accumulate parent stack as well for propagation up the vtree
     */
function delegateEvent
  ( event: Event
  , obj: VTree
  , stack: Array<HTMLElement>
  , parentStack: Array<VTree>
  , debug: boolean,
  ) {
  /* base case, not found */
  if (!stack.length) {
    if (debug) {
      console.warn(
        'Event "' +
          event.type +
          '" did not find an event handler to dispatch on',
        obj,
        event,
      );
    }
    return;
  } /* stack not length 1, recurse */ else if (stack.length > 1) {
    parentStack.unshift(obj);
    for (const child of obj['children']) {
      if (child['type'] === 'vcomp') continue;
        if (child['domRef'] === stack[1]) {
        delegateEvent(
          event,
          child,
          stack.slice(1),
          parentStack,
          debug,
        );
        break;
      }
    }
  } /* stack.length == 1 */ else {
    var eventObj : EventObject = obj['events'][event.type];
    if (eventObj) {
      var options : Options = eventObj['options'];
      if (options['preventDefault']) {
        event.preventDefault();
      }
      eventObj['runEvent'](event);
      if (!options['stopPropagation']) {
        propagateWhileAble(parentStack, event);
      }
    } else {
      /* still propagate to parent handlers even if event not defined */
      propagateWhileAble(parentStack, event);
    }
  }
};
/* Propagate the event up the chain, invoking other event handlers as encountered */
function propagateWhileAble (parentStack: Array<VTree>, event: Event) {
  for (const vtree of parentStack) {
    if (vtree['events'][event.type]) {
      var eventObj = vtree['events'][event.type],
        options = eventObj['options'];
      if (options['preventDefault']) event.preventDefault();
      eventObj['runEvent'](event);
      if (options['stopPropagation']) {
        event.stopPropagation();
        break;
      }
    }
  }
};
/* Walks down obj following the path described by `at`, then filters primitive
       values (string, numbers and booleans). Sort of like JSON.stringify(), but
       on an Event that is stripped of impure references.
    */
export function eventJSON(at: any, obj: VTree): any {
  /* If at is of type [[MisoString]] */
  if (typeof at[0] === 'object') {
    var ret = [];
    for (var i : any = 0; i < at.length; i++) {
      ret.push(eventJSON(at[i], obj));
    }
    return ret;
  }
  for (i in at) obj = obj[at[i]];
  /* If obj is a list-like object */
  var newObj;
  if (obj instanceof Array || ('length' in obj && obj['localName'] !== 'select')) {
    newObj = [];
    for (i = 0; i < obj.length; i++) {
      newObj.push(eventJSON([], obj[i]));
    }
    return newObj;
  }
  /* If obj is a non-list-like object */
  newObj = {};
  for (i in getAllPropertyNames(obj)) {
    /* bug in safari, throws TypeError if the following fields are referenced on a checkbox */
    /* https://stackoverflow.com/a/25569117/453261 */
    /* https://html.spec.whatwg.org/multipage/input.html#do-not-apply */
    if (
      obj['localName'] === 'input' &&
      (i === 'selectionDirection' ||
        i === 'selectionStart' ||
        i === 'selectionEnd')
    ) {
      continue;
    }
    if (
      typeof obj[i] == 'string' ||
      typeof obj[i] == 'number' ||
      typeof obj[i] == 'boolean'
    ) {
      newObj[i] = obj[i];
    }
  }
  return newObj;
}
/* get static and dynamic properties */
var getAllPropertyNames = function (obj: VTree) {
  var props: Object = {}, i: number = 0;
  do {
    var names = Object.getOwnPropertyNames(obj);
    for (i = 0; i < names.length; i++) {
      props[names[i]] = null;
    }
  } while ((obj = Object.getPrototypeOf(obj)));
  return props;
};
