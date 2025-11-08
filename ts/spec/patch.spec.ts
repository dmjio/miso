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
    SetTextContent,
    SetAttribute,
    SetAttributeNS,
    SetInlineStyle,
    RemoveAttribute,
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
    test('Should process the CreateTextNode patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
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
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
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
        expect(getPatches()).toEqual([expectedCreateElement, appendOperation, expectedSetAttribute]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
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
        expect(getPatches()).toEqual([expectedCreateElement, appendOperation, expectedSetAttribute]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
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
        expect(getPatches()).toEqual([expectedCreateElement, appendOperation, expectedSetAttribute]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
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
        expect(getPatches()).toEqual([expectedCreateElement, appendOperation, expectedSetAttribute, expectedRemoveAttribute]);
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
        expect(getPatches()).toEqual([expected, appendOperation, expectedSetTextContent]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
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
        expect(getPatches()).toEqual([expected, appendOperation1, expectedChild, appendOperation2 ]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0: document.body },
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
        let tree : VNode<NodeId> = vnode({ tag: 'p', css : { 'background-color': 'red' }});
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
            new : { 'background-color' : 'red' },
            current : {},
            componentId,
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation, expectedStyle]);
        let components : Components<DOMRef> = {};
        let component : Component<DOMRef> = {
            model: null,
            nodes: { 0 : document.body },
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
        expect(component.nodes[nodeId].style['background-color']).toEqual('red');
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
    });

    // test('Should process the swapDOMRefs patch', () => {
    //     expect(2+2).toEqual(4);
    // });

    // test('Should process the replaceChild patch', () => {
    //     expect(2+2).toEqual(4);
    // });

    // test('Should process the insertBefore patch', () => {
    //     expect(2+2).toEqual(4);
    // });

});
