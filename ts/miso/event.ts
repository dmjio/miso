import { EventContext, VTree, EventCapture, EventObject, Options, VTreeType } from './types';

/* event delegation algorithm */
export function delegate<T> (
  mount: T,
  events: Array<EventCapture>,
  getVTree: (vtree: VTree<T>) => void,
  debug: boolean,
  context: EventContext<T>,
): void {

  for (const event of events) {
   context.addEventListener (
      mount,
      event.name,
      function (e: Event) {
        listener(e, mount, getVTree, debug, context);
      },
      event.capture,
    );
  }
}
/* event undelegation */
export function undelegate<T> (
  mount: T,
  events: Array<EventCapture>,
  getVTree: (vtree: VTree<T>) => void,
  debug: boolean,
  context: EventContext<T>,
): void {
  for (const event of events) {
    context.removeEventListener (
      mount,
      event.name,
      function (e: Event) {
        listener(e, mount, getVTree, debug, context);
      },
      event.capture,
    );
  }
}
/* the event listener shared by both delegator and undelegator */
function listener<T>(e: Event | [Event], mount: T, getVTree: (VTree) => void, debug: boolean, context: EventContext<T>): void {
  getVTree(function (vtree: VTree<T>) {
      if (Array.isArray(e)) {
          for (const key of e) {
            dispatch (key, vtree, mount, debug, context);
          }
      } else {
          dispatch (e, vtree, mount, debug, context);
      }
  });
}

function dispatch <T> (ev: Event, vtree : VTree<T>, mount: T, debug: boolean, context : EventContext<T>) {
  var target = context.getTarget(ev);
  if (target) {
     let stack = buildTargetToElement(mount, target, context);
     delegateEvent(ev, vtree, stack, debug, context);
   }
}

/* Create a stack of ancestors used to index into the virtual DOM */
function buildTargetToElement<T>(element: T, target: T, context: EventContext<T>): Array<T> {
  var stack = [];
  while (!context.isEqual(element, target)) {
    stack.unshift(target);
    if (target && context.parentNode(target)) {
      target = context.parentNode(target);
    } else {
      return stack;
    }
  }
  return stack;
}
/* Finds event in virtual dom via pointer equality
   Accumulate parent stack as well for propagation up the vtree
*/
function delegateEvent <T>(
  event: Event,
  obj: VTree<T>,
  stack: Array<T>,
  debug: boolean,
  context: EventContext<T>,
): void {
  /* base case, not found */
  if (!stack.length) {
    if (debug) {
      console.warn(
        'Event "' + event.type + '" did not find an event handler to dispatch on',
        obj,
        event,
      );
    }
    return;
  } /* stack not length 1, recurse */
  else if (stack.length > 1) {
      if (obj.type === VTreeType.VText) {
        return;
      }
      else if (obj.type === VTreeType.VComp) {
        if (!obj.child) {
          if (debug) {
            console.error('VComp has no child property set during event delegation', obj);
            console.error('This means the Component has not been fully mounted, this should never happen');
            throw new Error('VComp has no .child property set during event delegation');
          }
          return;
        }
        return delegateEvent(event, obj.child, stack, debug, context);
      }
      else if (obj.type === VTreeType.VNode) {
        if (context.isEqual(obj.domRef, stack[0])) {
          const eventObj: EventObject<T> = obj.events.captures[event.type];
          if (eventObj) {
            const options: Options = eventObj.options;
            if (options.preventDefault) event.preventDefault();
            if (!event['captureStopped']) {
              eventObj.runEvent(event, obj.domRef);
            }
            if (options.stopPropagation) {
               /* If stopPropagation set, stop capturing */
               event['captureStopped'] = true;
            }
          }
          stack.splice(0,1);
        }
        for (const child of obj.children) {
          delegateEvent(event, child, stack, debug, context);
        }
      }
    } else {
    /* stack.length === 1, we're at the target */
    if (obj.type === VTreeType.VComp) {
      /* VComp doesn't have events directly, delegate to its child */
      if (obj.child) {
        delegateEvent(event, obj.child, stack, debug, context);
      }
    } else if (obj.type === VTreeType.VNode) {
    /* captures run first */
      const eventCaptureObj: EventObject<T> = obj.events.captures[event.type];
      if (eventCaptureObj && !event['captureStopped']) {
        const options: Options = eventCaptureObj.options;
        /* dmj: stack[0] represents the domRef that raised the event, this is the found case */
        if (context.isEqual(stack[0], obj.domRef)) {
          if (options.preventDefault) event.preventDefault();
          eventCaptureObj.runEvent(event, stack[0]);
          if (options.stopPropagation) event['captureStopped'] = true;
        }
      }
      /* bubble runs second, and propagates */
      const eventObj: EventObject<T> = obj.events.bubbles[event.type];
      if (eventObj && !event['captureStopped']) {
        const options: Options = eventObj.options;
        /* dmj: stack[0] represents the domRef that raised the event, this is the found case */
        if (context.isEqual(stack[0], obj.domRef)) {
          if (options.preventDefault) event.preventDefault();
          eventObj.runEvent(event, stack[0]);
          if (!options.stopPropagation) {
            propagateWhileAble(obj.parent, event);
          }
        }
      } else {
         /* still propagate to parent handlers even if event not defined */
          if (!event['captureStopped']) {
            propagateWhileAble(obj.parent, event);
          }
      }
    }
  }
}
/* Propagate the event up the chain, invoking other event handlers as encountered */
function propagateWhileAble<T>(vtree: VTree<T>, event: Event): void {
  while (vtree) {
    switch (vtree.type) {
      case VTreeType.VText:
        /* impossible case */
        break;
      case VTreeType.VNode:
        const eventObj = vtree.events.bubbles[event.type];
        if (eventObj) {
          const options = eventObj.options;
          if (options.preventDefault) event.preventDefault();
          eventObj.runEvent(event, vtree.domRef);
          if (options.stopPropagation) {
             /* if stop propagation set, stop bubbling */
             return;
           }
        }
        vtree = vtree.parent;
        break;
      case VTreeType.VComp:
        /* We've reached the Component barrier, bail if disallowed */
        if (!vtree.eventPropagation) return;
        vtree = vtree.parent;
        break;
    }
  }
}
/* Walks down obj following the path described by `at`, then filters primitive
       values (string, numbers and booleans). Sort of like JSON.stringify(), but
       on an Event that is stripped of impure references.
    */
export function eventJSON(at: string | Array<string>, obj: any): Object[] {
  /* If at is of type [[MisoString]] */
  if (typeof at[0] === 'object') {
    var ret = [];
    for (var i: number = 0; i < at.length; i++) {
      ret.push(eventJSON(at[i], obj));
    }
    return ret;
  }
  for (const a of at) obj = obj[a];
  /* If obj is a list-like object */
  var newObj;
  if (obj instanceof Array || ('length' in obj && obj['localName'] !== 'select')) {
    newObj = [];
    for (var j = 0; j < obj.length; j++) {
      newObj.push(eventJSON([], obj[j]));
    }
    return newObj;
  }
  /* If obj is a non-list-like object */
  newObj = {};
  for (var key in getAllPropertyNames(obj)) {
    /* bug in safari, throws TypeError if the following fields are referenced on a checkbox */
    /* https://stackoverflow.com/a/25569117/453261 */
    /* https://html.spec.whatwg.org/multipage/input.html#do-not-apply */
    if (
      obj['localName'] === 'input' &&
      (key === 'selectionDirection' || key === 'selectionStart' || key === 'selectionEnd')
    ) {
      continue;
    }
    if (
      typeof obj[key] == 'string' ||
      typeof obj[key] == 'number' ||
      typeof obj[key] == 'boolean'
    ) {
      newObj[key] = obj[key];
    }
  }
  return newObj;
}
/* get static and dynamic properties */
function getAllPropertyNames(obj: Event): Object {
  var props: Object = {},
    i: number = 0;
  do {
    var names = Object.getOwnPropertyNames(obj);
    for (i = 0; i < names.length; i++) {
      props[names[i]] = null;
    }
  } while ((obj = Object.getPrototypeOf(obj)));
  return props;
}
