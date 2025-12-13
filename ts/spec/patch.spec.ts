/* imports */
import { diff } from '../miso/dom';
import { vtext, vnode } from '../miso/smart';
import { VNode, DOMRef, DrawingContext, NodeId, VText } from '../miso/types';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
// import { context } from '../miso/context/dom';
import {
    patch,
    CreateElement,
    CreateElementNS,
    CreateTextNode,
    AppendChild,
    RemoveChild,
    InsertBefore,
    ReplaceChild,
    SetTextContent,
    SetAttribute,
    SetAttributeNS,
    SetInlineStyle,
    RemoveAttribute,
    AddClass,
    RemoveClass,
    Components,
    Component
} from '../miso/patch';
import { patchDrawingContext, getPatches } from '../miso/context/patch';
import { drawingContext } from '../miso/context/dom';

/* silence */
beforeAll(() => {
  console.log = () => {};
  console.info = () => {};
  console.warn = () => {};
  console.error = () => {};
  globalThis['componentId'] = 0;
  globalThis['nodeId'] = 1;
  globalThis['patches'] = [];
});

/* reset DOM */
afterEach(() => {
  document.body.innerHTML = '';
  globalThis['patches'] = [];
  globalThis['nodeId'] = 1;
});

/* tests */
describe ('Patch tests', () => {
    test('Should process Mount and Unmount patches', () => {
        const componentId: number = 0;
        const mountPoint: number = 0;
        const events: any[] = [];
        const domContext: DrawingContext<DOMRef> = drawingContext;
        let components: Components<DOMRef> = {};
        const mountPatch = {
            type: "mount" as const,
            componentId,
            events,
            model: {},
            mountPoint
        };
        const unmountPatch = {
            type: "unmount" as const,
            componentId
        };
        patch(domContext, mountPatch, components);
        expect(components[componentId].mountPoint).toEqual(mountPoint);
        expect(Array.isArray(components[componentId].events)).toBeTrue();
        patch(domContext, unmountPatch, components);
        expect(components[componentId]).toEqual(undefined);
    });

    test('Should process ModelHydration patch', () => {
        const componentId: number = 0;
        const domContext: DrawingContext<DOMRef> = drawingContext;
        let components: Components<DOMRef> = {};
        // mount component first
        patch(domContext, { type: "mount", componentId, events: [], model: {}, mountPoint: 0 }, components);
        const hydration = {
            type: "modelHydration" as const,
            componentId,
            model: { foo: 'bar' }
        };
        patch(domContext, hydration, components);
        expect(components[componentId].model).toEqual({ foo: 'bar' });
    });

    test('Should process SwapDOMRefs patch', () => {
        const componentId: number = 0;
        const parentNodeId: number = 0;
        const nodeAId: number = 1;
        const nodeBId: number = 2;
        const domContext: DrawingContext<DOMRef> = drawingContext;
        let components: Components<DOMRef> = {};
        const component: Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        // create two child nodes and append in order A, B
        const createA: CreateElement = { type: "createElement", tag: 'div', nodeId: nodeAId, componentId };
        const createB: CreateElement = { type: "createElement", tag: 'span', nodeId: nodeBId, componentId };
        const appendA: AppendChild = { type: "appendChild", parent: parentNodeId, child: nodeAId, componentId };
        const appendB: AppendChild = { type: "appendChild", parent: parentNodeId, child: nodeBId, componentId };
        patch(domContext, createA, components);
        patch(domContext, createB, components);
        patch(domContext, appendA, components);
        patch(domContext, appendB, components);
        expect(document.body.childNodes[0].nodeName).toEqual('DIV');
        expect(document.body.childNodes[1].nodeName).toEqual('SPAN');
        const swapPatch = { type: "swapDOMRefs" as const, componentId, nodeA: nodeAId, nodeB: nodeBId, parent: parentNodeId };
        patch(domContext, swapPatch, components);
        // ensure function executes and children remain present
        expect(document.body.children.length).toEqual(2);
        // ensure component nodes still reference correct elements
        expect(component.nodes[nodeAId].nodeName).toEqual('DIV');
        expect(component.nodes[nodeBId].nodeName).toEqual('SPAN');
    });

    test('SwapDOMRefs with adjacent siblings', () => {
        const componentId: number = 0;
        const parentNodeId: number = 0;
        const nodeAId: number = 1;
        const nodeBId: number = 2;
        const domContext: DrawingContext<DOMRef> = drawingContext;
        let components: Components<DOMRef> = {};
        const component: Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        const createA: CreateElement = { type: "createElement", tag: 'div', nodeId: nodeAId, componentId };
        const createB: CreateElement = { type: "createElement", tag: 'span', nodeId: nodeBId, componentId };
        const appendA: AppendChild = { type: "appendChild", parent: parentNodeId, child: nodeAId, componentId };
        const appendB: AppendChild = { type: "appendChild", parent: parentNodeId, child: nodeBId, componentId };
        patch(domContext, createA, components);
        patch(domContext, createB, components);
        patch(domContext, appendA, components);
        patch(domContext, appendB, components);
        expect(document.body.childNodes[0].nodeName).toEqual('DIV');
        expect(document.body.childNodes[1].nodeName).toEqual('SPAN');
        const swapPatch = { type: "swapDOMRefs" as const, componentId, nodeA: nodeAId, nodeB: nodeBId, parent: parentNodeId };
        patch(domContext, swapPatch, components);
        expect(document.body.childNodes[0].nodeName).toEqual('SPAN');
        expect(document.body.childNodes[1].nodeName).toEqual('DIV');
    });

    test('SwapDOMRefs with non-adjacent siblings', () => {
        const componentId: number = 0;
        const parentNodeId: number = 0;
        const nodeAId: number = 1;
        const nodeMidId: number = 2;
        const nodeBId: number = 3;
        const domContext: DrawingContext<DOMRef> = drawingContext;
        let components: Components<DOMRef> = {};
        const component: Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        const createA: CreateElement = { type: "createElement", tag: 'div', nodeId: nodeAId, componentId };
        const createMid: CreateElement = { type: "createElement", tag: 'em', nodeId: nodeMidId, componentId };
        const createB: CreateElement = { type: "createElement", tag: 'span', nodeId: nodeBId, componentId };
        const appendA: AppendChild = { type: "appendChild", parent: parentNodeId, child: nodeAId, componentId };
        const appendMid: AppendChild = { type: "appendChild", parent: parentNodeId, child: nodeMidId, componentId };
        const appendB: AppendChild = { type: "appendChild", parent: parentNodeId, child: nodeBId, componentId };
        patch(domContext, createA, components);
        patch(domContext, createMid, components);
        patch(domContext, createB, components);
        patch(domContext, appendA, components);
        patch(domContext, appendMid, components);
        patch(domContext, appendB, components);
        expect(Array.from(document.body.childNodes).map(n => n.nodeName)).toEqual(['DIV','EM','SPAN']);
        const swapPatch = { type: "swapDOMRefs" as const, componentId, nodeA: nodeAId, nodeB: nodeBId, parent: parentNodeId };
        patch(domContext, swapPatch, components);
        expect(Array.from(document.body.childNodes).map(n => n.nodeName)).toEqual(['SPAN','EM','DIV']);
    });

    test('Patch.ts swapDOMRefs branch executes', () => {
        const componentId: number = 0;
        const parentNodeId: number = 0;
        const aId: number = 1;
        const bId: number = 2;
        const domContext: DrawingContext<DOMRef> = drawingContext;
        let components: Components<DOMRef> = {};
        const component: Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        const createA: CreateElement = { type: "createElement", tag: 'div', nodeId: aId, componentId };
        const createB: CreateElement = { type: "createElement", tag: 'span', nodeId: bId, componentId };
        const appendA: AppendChild = { type: "appendChild", parent: parentNodeId, child: aId, componentId };
        const appendB: AppendChild = { type: "appendChild", parent: parentNodeId, child: bId, componentId };
        patch(domContext, createA, components);
        patch(domContext, createB, components);
        patch(domContext, appendA, components);
        patch(domContext, appendB, components);
        const op = { type: "swapDOMRefs" as const, componentId, nodeA: aId, nodeB: bId, parent: parentNodeId };
        patch(domContext, op, components);
        // ensure executed and order changed
        expect(document.body.firstChild.nodeName).toEqual('SPAN');
        expect(document.body.childNodes[1].nodeName).toEqual('DIV');
    });

    test('SetInlineStyle handles updates and removals', () => {
        const componentId: number = 0;
        const parentNodeId: number = 0;
        const nodeId: number = 1;
        const domContext: DrawingContext<DOMRef> = drawingContext;
        let components: Components<DOMRef> = {};
        const component: Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        const create: CreateElement = { type: "createElement", tag: 'p', nodeId, componentId };
        const append: AppendChild = { type: "appendChild", parent: parentNodeId, child: nodeId, componentId };
        patch(domContext, create, components);
        patch(domContext, append, components);
        const initialStyle: SetInlineStyle = { type: "setInlineStyle", current: {}, new: { color: 'red', backgroundColor: 'white' }, nodeId, componentId };
        patch(domContext, initialStyle, components);
        expect(component.nodes[nodeId].style['color']).toEqual('red');
        expect(component.nodes[nodeId].style['backgroundColor']).toEqual('white');
        // Update color, remove backgroundColor
        const updateStyle: SetInlineStyle = { type: "setInlineStyle", current: { color: 'red', backgroundColor: 'white' }, new: { color: 'blue' }, nodeId, componentId };
        patch(domContext, updateStyle, components);
        expect(component.nodes[nodeId].style['color']).toEqual('blue');
        expect(component.nodes[nodeId].style['backgroundColor']).toEqual('');
    });
    test('Should process the CreateTextNode patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        const componentId : number = 0;
        let tree : VText<NodeId> = vtext('foo');
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateTextNode = {
            text : 'foo',
            type : "createTextNode",
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expected, components);
        // patch (domContext, appendOperation, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.children.length).toEqual(0); //dmj: doesn't get appended
        expect(component.nodes[parentNodeId]).toEqual(document.body);
        expect(component.nodes[nodeId].textContent).toEqual('foo');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(Object.keys(component.nodes).length).toEqual(2);
        patch (domContext, appendOperation, components);
        expect(document.body.childNodes.length).toEqual(1); //dmj: gets appended
        expect(document.body.childNodes[0].textContent).toEqual('foo');
    });
    test('Should process the CreateElement patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        const componentId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'p' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expected, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(0); //dmj: doesn't get appended
        expect(component.nodes[nodeId].nodeName).toEqual('P');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);

        patch (domContext, appendOperation, components);
        expect(document.body.childNodes.length).toEqual(1); //dmj: gets appended
        expect(document.body.childNodes[0].nodeName).toEqual('P');
    });
    test('Should process the CreateElementNS patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        const componentId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'svg', ns: 'svg' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateElementNS = {
            tag: 'svg',
            type : "createElementNS",
            namespace: 'http://www.w3.org/2000/svg',
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expected, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.children.length).toEqual(0); //dmj: doesn't get appended
        expect(component.nodes[nodeId].namespaceURI).toEqual('http://www.w3.org/2000/svg');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);

        patch (domContext, appendOperation, components);
        expect(document.body.childNodes.length).toEqual(1); //dmj: gets appended
        expect(document.body.childNodes[0].nodeName).toEqual('svg');
    });
    test('Should process the SetAttribute patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        const componentId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'p', props: { tabIndex: 0 }});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expectedCreateElement : CreateElement = {
            tag: 'p',
            type : "createElement",
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedSetAttribute : SetAttribute = {
            key: 'tabIndex',
            value: 0,
            type : "setAttribute",
            componentId,
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expectedCreateElement, expectedSetAttribute, appendOperation]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expectedCreateElement, components);
        patch (domContext, appendOperation, components);
        patch (domContext, expectedSetAttribute, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: doesn't get appended
        expect(component.nodes[nodeId].nodeName).toEqual('P');
        expect(component.nodes[nodeId].tabIndex).toEqual(0);
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
    });
    test('Should process the SetAttributeNS patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        const componentId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'svg', props: { href: 'google.com' }, ns: 'svg' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expectedCreateElement : CreateElementNS = {
            tag: 'svg',
            type : "createElementNS",
            namespace: 'http://www.w3.org/2000/svg',
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedSetAttribute : SetAttributeNS = {
            key: 'href',
            value: 'google.com',
            namespace: 'http://www.w3.org/1999/xlink',
            type : "setAttributeNS",
            componentId,
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expectedCreateElement, expectedSetAttribute, appendOperation ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expectedCreateElement, components);
        patch (domContext, appendOperation, components);
        patch (domContext, expectedSetAttribute, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: doesn't get appended
        expect(component.nodes[nodeId].nodeName).toEqual('svg');
        // expect(component.nodes[nodeId]['href']).toEqual('google.com'); // dmj --- hrmmm?
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
    });
    test('Should process the RemoveAttribute patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        const componentId : number = 0;
        const tree : VNode<NodeId> = vnode({ tag: 'p', props: { foo: 'bar' }});
        const newTree : VNode<NodeId> = vnode({ tag: 'p' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expectedCreateElement : CreateElement = {
            tag: 'p',
            type : "createElement",
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedSetAttribute : SetAttribute = {
            key: 'foo',
            value: 'bar',
            type : "setAttribute",
            componentId,
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expectedCreateElement, expectedSetAttribute, appendOperation ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expectedCreateElement, components);
        patch (domContext, appendOperation, components);
        patch (domContext, expectedSetAttribute, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: doesn't get appended
        expect(component.nodes[nodeId].nodeName).toEqual('P');
        expect(component.nodes[nodeId].getAttribute('foo')).toEqual('bar');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
        let expectedRemoveAttribute : RemoveAttribute = {
            key: 'foo',
            type : "removeAttribute",
            componentId,
            nodeId,
        };
        diff (tree, newTree, parent, patchContext);
        expect(getPatches()).toEqual([expectedCreateElement, expectedSetAttribute, appendOperation, expectedRemoveAttribute]);
        patch (domContext, expectedRemoveAttribute, components);
        expect(component.nodes[nodeId].getAttribute('foo')).toEqual(null);
    });
    test('Should process the SetTextContent patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        const componentId : number = 0;
        let tree : VText<NodeId> = vtext('foo');
        let newTree : VText<NodeId> = vtext('bar');
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        diff (tree, newTree, parent, patchContext);
        let expected : CreateTextNode = {
            text : 'foo',
            type : "createTextNode",
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedSetTextContent : SetTextContent = {
            text : 'bar',
            type : "setTextContent",
            componentId,
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation, expectedSetTextContent ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expected, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(0); //dmj: doesn't get appended
        expect(component.nodes[nodeId].textContent).toEqual('foo');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);

        patch (domContext, appendOperation, components);
        patch (domContext, expectedSetTextContent, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(component.nodes[nodeId].textContent).toEqual('bar');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
    });

    test('Should process the AppendChild patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const nodeIdChild : number = 2;
        const componentId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'a' })]});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            componentId,
            nodeId,
        };
        let appendOperation1 : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedChild : CreateElement = {
            type : "createElement",
            tag: 'a',
            nodeId: nodeIdChild,
            componentId,
        };
        let appendOperation2 : AppendChild = {
            type : "appendChild",
            parent: nodeId,
            child: nodeIdChild,
            componentId
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, expectedChild, appendOperation2, appendOperation1 ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        }; 
        components[componentId] = component;
        patch (domContext, expected, components);
        patch (domContext, appendOperation1, components);
        patch (domContext, expectedChild, components);
        patch (domContext, appendOperation2, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(component.nodes[nodeId].nodeName).toEqual('P');
        expect(component.nodes[nodeId].childNodes[0].nodeName).toEqual('A');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(component.nodes[nodeIdChild]['nodeId']).toEqual(nodeIdChild);
    });

    test('Should process the SetInlineStyle patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const componentId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'p', css : { 'backgroundColor': 'red' }});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedStyle : SetInlineStyle = {
            type : "setInlineStyle",
            new : { 'backgroundColor' : 'red' },
            current : {},
            componentId,
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, expectedStyle, appendOperation ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
            events: null,
            mountPoint: null
        }; 
        components[componentId] = component;
        patch (domContext, expected, components);
        patch (domContext, appendOperation, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(component.nodes[nodeId].nodeName).toEqual('P');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);

        // dmj: check that the style gets applied
        patch (domContext, expectedStyle, components);
        expect(component.nodes[nodeId].style['backgroundColor']).toEqual('red');
    });
    test('Should process the Flush patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const componentId : number = 0;
        let tree : VText<NodeId> = vtext('foo');
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateTextNode = {
            text : 'foo',
            type : "createTextNode",
            componentId,
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };

        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation]);
        patchContext.flush ();
        expect(getPatches().length).toEqual(0);

        // also exercise flush branch in patch.ts
        const components : Components<DOMRef> = {};
        const component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        const flushPatch = { type: "flush" as const, componentId };
        // Before calling flush via patch.ts, add something to DOM to verify it remains intact
        const textCreate : CreateTextNode = { type: "createTextNode", text: 'bar', nodeId: 2, componentId };
        const textAppend : AppendChild = { type: "appendChild", parent: 0, child: 2, componentId };
        patch(domContext, textCreate, components);
        patch(domContext, textAppend, components);
        expect(document.body.childNodes.length).toBeGreaterThan(0);
        patch(domContext, flushPatch, components);
        // flush has no DOM effect; ensure still intact
        expect(document.body.childNodes.length).toBeGreaterThan(0);
    });

    test('Patch.ts should handle missing component (withComponent else branch)', () => {
        const domContext: DrawingContext<DOMRef> = drawingContext;
        const components: Components<DOMRef> = {};
        const badComponentId = 1234;
        const op = { type: "flush" as const, componentId: badComponentId };
        expect(() => patch(domContext, op, components)).not.toThrow();
    });

     test('Should process the ReplaceChild patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const nodeIdChild1 : number = 2;
        const nodeIdChild2 : number = 3;
        const componentId : number = 0;
        let firstTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'a' })]});
        let secondTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'img' })]});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, firstTree, parent, patchContext);
        diff (firstTree, secondTree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            componentId,
            nodeId,
        };
        let appendOperation1 : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedChild : CreateElement = {
            type : "createElement",
            tag: 'a',
            nodeId: nodeIdChild1,
            componentId,
        };
        let appendOperation2 : AppendChild = {
            type : "appendChild",
            parent: nodeId,
            child: nodeIdChild1,
            componentId
        };
        let newNode : CreateElement = {
            tag: 'img',
            type : "createElement",
            componentId,
            nodeId: nodeIdChild2,
        };
        let replaceOp : ReplaceChild = {
            type : "replaceChild",
            parent: nodeId,
            current: nodeIdChild1,
            new: nodeIdChild2,
            componentId
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, expectedChild, appendOperation2, appendOperation1, newNode, replaceOp ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expected, components);
        patch (domContext, appendOperation1, components);
        patch (domContext, expectedChild, components);
        patch (domContext, appendOperation2, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(component.nodes[nodeId].nodeName).toEqual('P');
        expect(component.nodes[nodeId].childNodes[0].nodeName).toEqual('A');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(component.nodes[nodeIdChild1].nodeName).toEqual('A');
        expect(component.nodes[nodeIdChild1]['nodeId']).toEqual(nodeIdChild1);

        patch (domContext, newNode, components);
        patch (domContext, replaceOp, components);
        expect(component.nodes[nodeId].childNodes[0].nodeName).toEqual('IMG');
        expect(component.nodes[nodeIdChild2].nodeName).toEqual('IMG');
        expect(component.nodes[nodeIdChild1]).toEqual(undefined);
        expect(component.nodes[nodeIdChild2]['nodeId']).toEqual(nodeIdChild2);
        
    });

  test('Should process the RemoveChild patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const nodeIdChild : number = 2;
        const componentId : number = 0;
        let firstTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'a' })]});
        let secondTree : VNode<NodeId> = vnode({ tag: 'p' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, firstTree, parent, patchContext);
        diff (firstTree, secondTree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            componentId,
            nodeId,
        };
        let appendOperation1 : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedChild : CreateElement = {
            type : "createElement",
            tag: 'a',
            nodeId: nodeIdChild,
            componentId,
        };
        let appendOperation2 : AppendChild = {
            type : "appendChild",
            parent: nodeId,
            child: nodeIdChild,
            componentId
        };
        let removeOp : RemoveChild = {
            type : "removeChild",
            componentId,
            child: nodeIdChild,
            parent: nodeId
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, expectedChild, appendOperation2, appendOperation1, removeOp ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expected, components);
        patch (domContext, appendOperation1, components);
        patch (domContext, expectedChild, components);
        patch (domContext, appendOperation2, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(component.nodes[nodeId].nodeName).toEqual('P');
        expect(component.nodes[nodeId].childNodes[0].nodeName).toEqual('A');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(component.nodes[nodeId].childNodes.length).toEqual(1);
        expect(component.nodes[nodeIdChild].nodeName).toEqual('A');
        expect(component.nodes[nodeIdChild]['nodeId']).toEqual(nodeIdChild);

        patch (domContext, removeOp, components);
        expect(component.nodes[nodeId].childNodes.length).toEqual(0);
        expect(component.nodes[nodeIdChild]).toEqual(undefined);

    });


  test('Should process the InsertBefore patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const nodeIdChild1 : number = 2;
        const nodeIdChild2 : number = 3;
        const componentId : number = 0;
        let firstTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'a', key: '1' }) ]});
        let secondTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'img', key: '2' }) ]});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, firstTree, parent, patchContext);
        diff (firstTree, secondTree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            componentId,
            nodeId,
        };
        let appendOperation1 : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
            componentId
        };
        let expectedChild1 : CreateElement = {
            type : "createElement",
            tag: 'a',
            nodeId: nodeIdChild1,
            componentId,
        };
        let appendOperation2 : AppendChild = {
            type : "appendChild",
            parent: nodeId,
            child: nodeIdChild1,
            componentId
        };
        let expectedChild2 : CreateElement = {
            type : "createElement",
            tag: 'img',
            nodeId: nodeIdChild2,
            componentId,
        };
        let insertBeforeOp : InsertBefore = {
            type : "insertBefore",
            parent: nodeId,
            node: nodeIdChild2,
            child: nodeIdChild1,
            componentId
        };
        let removeChildOp : RemoveChild = {
            type : "removeChild",
            componentId,
            child: nodeIdChild1,
            parent: nodeId
        };
        // dmj: check diff produces patch object
      expect(getPatches()).toEqual([expected, expectedChild1, appendOperation2, appendOperation1, expectedChild2, insertBeforeOp, removeChildOp ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, expected, components);
        patch (domContext, appendOperation1, components);
        patch (domContext, expectedChild1, components);
        patch (domContext, appendOperation2, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(component.nodes[nodeId].nodeName).toEqual('P');
        expect(component.nodes[nodeId].childNodes[0].nodeName).toEqual('A');
        expect(component.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(component.nodes[nodeId].childNodes.length).toEqual(1);
        expect(component.nodes[nodeIdChild1].nodeName).toEqual('A');
        expect(component.nodes[nodeIdChild1]['nodeId']).toEqual(nodeIdChild1);

        patch (domContext, expectedChild2, components);
        patch (domContext, insertBeforeOp, components);
        expect(component.nodes[nodeId].childNodes.length).toEqual(2);
        expect(document.body.firstChild.childNodes[1].previousSibling.nodeName).toEqual('IMG');

        patch (domContext, removeChildOp, components);
        expect(component.nodes[nodeIdChild1]).toEqual(undefined);
        expect(component.nodes[nodeIdChild2].nodeName).toEqual('IMG');
    });

  test('Should process the AddClass patch', () => {
        const parentNodeId : number = 0;
        const componentId : number = 0;
        const nodeId : number = 1;
        let vtree = vnode({});
        vtree.classList = new Set(["foo", "bar"]);
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, vtree, parent, patchContext);
        let child : CreateElement = {
            type : "createElement",
            tag: 'div',
            nodeId,
            componentId,
        };
        let patch1 : AddClass = {
            key: 'foo',
            type : "addClass",
            componentId,
            nodeId,
        };
        let patch2 : AddClass = {
            key: 'bar',
            type : "addClass",
            componentId,
            nodeId,
        };
        let appendPatch : AppendChild = {
           child: 1,
           componentId: 0,
           parent: 0,
           type: "appendChild",
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([child, patch1, patch2, appendPatch]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, child, components);
        patch (domContext, patch1, components);
        patch (domContext, patch2, components);
        patch (domContext, appendPatch, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes[0]['className']).toEqual("foo bar");
        expect(component.nodes[nodeId].nodeName).toEqual('DIV');
    });

  test('Should process the RemoveClass patch', () => {
        const parentNodeId : number = 0;
        const componentId : number = 0;
        const nodeId : number = 1;
        let vtree1 = vnode({});
        vtree1.classList = new Set(["foo", "bar"]);
        let vtree2 = vnode({});
        vtree2.classList = new Set(["foo"]);
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId: parentNodeId };
        diff (null, vtree1, parent, patchContext);
        diff (vtree1, vtree2, parent, patchContext);
        let child : CreateElement = {
            type : "createElement",
            tag: 'div',
            nodeId,
            componentId,
        };
        let patch1 : AddClass = {
            key: 'foo',
            type : "addClass",
            componentId,
            nodeId,
        };
        let patch2 : AddClass = {
            key: 'bar',
            type : "addClass",
            componentId,
            nodeId,
        };
        let appendPatch : AppendChild = {
           child: 1,
           componentId: 0,
           parent: 0,
           type: "appendChild",
        };
        let patch3 : RemoveClass = {
            key: 'bar',
            type : "removeClass",
            componentId,
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([child, patch1, patch2, appendPatch, patch3]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
            events: null,
            mountPoint: null
        };
        components[componentId] = component;
        patch (domContext, child, components);
        patch (domContext, patch1, components);
        patch (domContext, patch2, components);
        patch (domContext, appendPatch, components);
        patch (domContext, patch3, components);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes[0]['className']).toEqual("foo");
        expect(component.nodes[nodeId].nodeName).toEqual('DIV');
    });


});
