import { EventContext, VTree, EventCapture, EventObject, Options, VTreeType } from './types';

/* event delegation algorithm */
export function delegator<T> (
  mount: T,
  events: Array<EventCapture>,
  getVTree: ((callback: (vtree : VTree<T>) => void) => void),
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
/* the event listener shared by both delegator and undelegator */
function listener<T>(e: Event | [Event], mount: T, getVTree: ((callback: (vtree: VTree<T>) => void) => void), debug: boolean, context: EventContext<T>): void {
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
export function delegateEvent <T> (
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
      else if (obj.type === VTreeType.VFrag) {
        // Walk into whichever child's subtree contains the target.
        // Guard with containsDOMRef before recursing so we don't mutate the
        // shared stack array for siblings that don't contain the target.
        for (const child of obj.children) {
          if (containsDOMRef(child, stack[0], context)) {
            delegateEvent(event, child, stack, debug, context);
            return;
          }
        }
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
          for (const child of obj.children) {
            if (containsDOMRef(child, stack[0], context)) {
              delegateEvent(event, child, stack, debug, context);
              return;
            }
          }
        }
        return;
      }
    } else {
    /* stack.length === 1, we're at the target */
    if (obj.type === VTreeType.VComp) {
      /* VComp doesn't have events directly, delegate to its child */
      if (obj.child) {
        delegateEvent(event, obj.child, stack, debug, context);
      }
    } else if (obj.type === VTreeType.VFrag) {
      /* VFrag doesn't have events directly, delegate into the child that owns the target */
      for (const child of obj.children) {
        if (containsDOMRef(child, stack[0], context)) {
          delegateEvent(event, child, stack, debug, context);
          return;
        }
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
      case VTreeType.VFrag:
        /* Propagate through fragment to its parent */
        vtree = vtree.parent;
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
/* Walk a sequence of property keys into an object */
function resolvePath(obj: any, path: Array<string>): any {
  return path.reduce((node, key) => node?.[key], obj);
}

/* True for array-like objects, but not DOM select elements */
function isSequence(obj: any): boolean {
  return Array.isArray(obj) || ('length' in obj && obj['localName'] !== 'select');
}

/* Safari throws TypeError reading these fields on a checkbox input:
   https://stackoverflow.com/a/25569117/453261
   https://html.spec.whatwg.org/multipage/input.html#do-not-apply */
const INPUT_SKIP = new Set(['selectionDirection', 'selectionStart', 'selectionEnd']);

/* Extract primitive-valued properties, walking the full prototype chain */
function serializePrimitives(obj: any): Record<string, string | number | boolean> {
  const out: Record<string, string | number | boolean> = {};
  for (const key of getAllPropertyNames(obj)) {
    if (obj['localName'] === 'input' && INPUT_SKIP.has(key)) continue;
    const v = obj[key];
    if (typeof v === 'string' || typeof v === 'number' || typeof v === 'boolean') out[key] = v;
  }
  return out;
}

/* Walks into obj via path `at`, then serializes the result.
   `at` is either a flat key path (string[]) or a list of paths (string[][]). */
export function eventJSON(at: Array<string> | Array<Array<string>>, obj: any): any {
  if (at.length > 0 && Array.isArray(at[0])) {
    return (at as Array<Array<string>>).map(path => eventJSON(path, obj));
  }
  const node = resolvePath(obj, at as Array<string>);
  if (isSequence(node)) {
    return Array.from({ length: node.length }, (_, i) => eventJSON([], node[i]));
  }
  return serializePrimitives(node);
}
/* Returns true if target is among the DOM refs owned by vtree (handles VFrag/VComp) */
function containsDOMRef<T>(vtree: VTree<T>, target: T, context: EventContext<T>): boolean {
  switch (vtree.type) {
    case VTreeType.VFrag:
      for (const child of vtree.children)
        if (containsDOMRef(child, target, context)) return true;
      return false;
    case VTreeType.VComp:
      return vtree.child ? containsDOMRef(vtree.child, target, context) : false;
    default:
      return context.isEqual(vtree.domRef, target);
  }
}
/* Collect all property names up the prototype chain */
function getAllPropertyNames(obj: any): Set<string> {
  const names = new Set<string>();
  let current = obj;
  do {
    for (const name of Object.getOwnPropertyNames(current)) names.add(name);
  } while ((current = Object.getPrototypeOf(current)));
  return names;
}
