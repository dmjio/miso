import { DrawingContext, PATCH, Runtime } from './types';

/* Function for patch application */
export function patch<T> (context: DrawingContext<T>, patch: PATCH, runtime: Runtime<T>) {
  switch (patch.type) {
    case "mount":
        runtime.components[patch.componentId] = {
          model : patch.model,
          mainThreadEvents : {},
          rootId : patch.mountPoint
        };
        break;
    case "unmount":
        delete runtime.components[patch.componentId];
        break;
    case "modelHydration":
        runtime.components[patch.componentId].model = patch.model;
        break;
    case "createElement":
        runtime.nodes[patch.nodeId] = context.createElement (patch.tag);
        runtime.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
        break;
    case "createElementNS":
        runtime.nodes[patch.nodeId] = context.createElementNS (patch.namespace, patch.tag);
        runtime.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
        break;
    case "createTextNode":
        runtime.nodes[patch.nodeId] = context.createTextNode (patch.text);
        runtime.nodes[patch.nodeId]['nodeId'] = patch.nodeId;
        break;
    case "setAttribute":
        context.setAttribute (runtime.nodes[patch.nodeId], patch.key, patch.value);
        break;
    case "addClass":
        context.addClass (patch.key, runtime.nodes[patch.nodeId]);
        break;
    case "removeClass":
        context.removeClass (patch.key, runtime.nodes[patch.nodeId]);
        break;
    case "setAttributeNS":
        context.setAttributeNS (runtime.nodes[patch.nodeId], patch.namespace, patch.key, patch.value)
        break;
    case "removeChild":
        context.removeChild (runtime.nodes[patch.parent], runtime.nodes[patch.child]);
        delete runtime.nodes[patch.child];
        break;
    case "appendChild":
        context.appendChild (runtime.nodes[patch.parent], runtime.nodes[patch.child]);
        break;
    case "setInlineStyle":
        context.setInlineStyle (patch.current, patch.new, runtime.nodes[patch.nodeId]);
        break;
    case "removeAttribute":
        context.removeAttribute (runtime.nodes[patch.nodeId], patch.key);
        break;
    case "setTextContent":
        context.setTextContent (runtime.nodes[patch.nodeId], patch.text);
        break;
    case "flush":
        context.flush ();
        break;
    case "insertBefore":
        context.insertBefore(runtime.nodes[patch.parent], runtime.nodes[patch.node], runtime.nodes[patch.child]);
        break;
    case "swapDOMRefs":
        /* dmj: swap it in the runtime environemnt too */
        context.swapDOMRefs (runtime.nodes[patch.nodeB], runtime.nodes[patch.nodeA], runtime.nodes[patch.parent]);
        break;
    case "replaceChild":
        context.replaceChild (runtime.nodes[patch.parent], runtime.nodes[patch.new], runtime.nodes[patch.current]);
        delete runtime.nodes[patch.current];
        break;
    default:
        break;
  }
}

