import { Context, VTree, EventCapture, EventObject, Options } from './types';

/* event delegation algorithm */
export function delegate<T> (
  mount: T,
  events: Array<EventCapture>,
  getVTree: (vtree: VTree<T>) => void,
  debug: boolean,
  context: Context<T>,
): void {

  for (const event of events) {
   context.addEventListener (
      mount,
      event['name'],
      function (e: Event) {
        listener(e, mount, getVTree, debug, context);
      },
      event['capture'],
    );
  }
}
/* event undelegation */
export function undelegate<T> (
  mount: T,
  events: Array<EventCapture>,
  getVTree: (vtree: VTree<T>) => void,
  debug: boolean,
  context: Context<T>,
): void {
  for (const event of events) {
    context.removeEventListener (
      mount,
      event['name'],
      function (e: Event) {
        listener(e, mount, getVTree, debug, context);
      },
      event['capture'],
    );
  }
}
/* the event listener shared by both delegator and undelegator */
function listener<T>(e: Event | [Event], mount: T, getVTree: (VTree) => void, debug: boolean, context: Context<T>): void {
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

function dispatch <T> (ev, vtree : VTree<T>, mount: T, debug: boolean, context : Context<T>) {
  var target = context['getTarget'](ev);
  if (target) {
     var stack = buildTargetToElement(mount, target, context);
     delegateEvent(ev, vtree, stack, [], debug, context);
   }
}

/* Create a stack of ancestors used to index into the virtual DOM */
function buildTargetToElement<T>(element: T, target: T, context: Context<T>): Array<T> {
  var stack = [];
  while (!context['isEqual'](element, target)) {
    stack.unshift(target);
    if (target && context['parentNode'](target)) {
      target = context['parentNode'](target);
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
  parentStack: Array<VTree<T>>,
  debug: boolean,
  context: Context<T>,
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
    parentStack.unshift(obj);
    for (var c in obj['children']) {
      var child = obj['children'][c];
      if (child['type'] === 'vcomp') continue;
      if (context['isEqual'](child['domRef'], stack[1])) {
        delegateEvent(event, child, stack.slice(1), parentStack, debug, context);
        break;
      }
    }
  } /* stack.length == 1 */
  else {
    const eventObj: EventObject<T> = obj['events'][event.type];
    if (eventObj) {
      const options: Options = eventObj['options'];
      if (options['preventDefault']) {
        event.preventDefault();
      }
      /* dmj: stack[0] represents the domRef that raised the event */
      eventObj['runEvent'](event, stack[0]);
      if (!options['stopPropagation']) {
        propagateWhileAble(parentStack, event);
      }
    } else {
      /* still propagate to parent handlers even if event not defined */
      propagateWhileAble(parentStack, event);
    }
  }
}
/* Propagate the event up the chain, invoking other event handlers as encountered */
function propagateWhileAble<T>(parentStack: Array<VTree<T>>, event: Event): void {
  for (const vtree of parentStack) {
    if (vtree['events'][event.type]) {
      const eventObj = vtree['events'][event.type],
        options = eventObj['options'];
      if (options['preventDefault']) event.preventDefault();
      eventObj['runEvent'](event, vtree['domRef']);
      if (options['stopPropagation']) {
        event.stopPropagation();
        break;
      }
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
