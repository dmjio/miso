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
    Runtime,
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
    test('Should expose getHead for patch drawing context', () => {
      expect(patchDrawingContext.getHead()).toEqual({ nodeId: 0});
    });

    test('Should process the CreateTextNode patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        let tree : VText<NodeId> = vtext('foo');
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateTextNode = {
            text : 'foo',
            type : "createTextNode",
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        // patch (domContext, appendOperation, runtimes);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.children.length).toEqual(0); //dmj: doesn't get appended
        expect(runtime.nodes[parentNodeId]).toEqual(document.body);
        expect(runtime.nodes[nodeId].textContent).toEqual('foo');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(Object.keys(runtime.nodes).length).toEqual(2);
        patch (domContext, appendOperation, runtime);
        expect(document.body.childNodes.length).toEqual(1); //dmj: gets appended
        expect(document.body.childNodes[0].textContent).toEqual('foo');
    });
    test('Should process the CreateElement patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'p' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(0); //dmj: doesn't get appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('P');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);

        patch (domContext, appendOperation, runtime);
        expect(document.body.childNodes.length).toEqual(1); //dmj: gets appended
        expect(document.body.childNodes[0].nodeName).toEqual('P');
    });
    test('Should process the CreateElementNS patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'svg', ns: 'svg' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateElementNS = {
            tag: 'svg',
            type : "createElementNS",
            namespace: 'http://www.w3.org/2000/svg',
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.children.length).toEqual(0); //dmj: doesn't get appended
        expect(runtime.nodes[nodeId].namespaceURI).toEqual('http://www.w3.org/2000/svg');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);

        patch (domContext, appendOperation, runtime);
        expect(document.body.childNodes.length).toEqual(1); //dmj: gets appended
        expect(document.body.childNodes[0].nodeName).toEqual('svg');
    });
    test('Should process the SetAttribute patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'p', props: { tabIndex: 0 }});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expectedCreateElement : CreateElement = {
            tag: 'p',
            type : "createElement",
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedSetAttribute : SetAttribute = {
            key: 'tabIndex',
            value: 0,
            type : "setAttribute",
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expectedCreateElement, expectedSetAttribute, appendOperation]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expectedCreateElement, runtime);
        patch (domContext, appendOperation, runtime);
        patch (domContext, expectedSetAttribute, runtime);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: doesn't get appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('P');
        expect(runtime.nodes[nodeId].tabIndex).toEqual(0);
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
    });
    test('Should process the SetAttributeNS patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        let tree : VNode<NodeId> = vnode({ tag: 'svg', props: { href: 'google.com' }, ns: 'svg' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expectedCreateElement : CreateElementNS = {
            tag: 'svg',
            type : "createElementNS",
            namespace: 'http://www.w3.org/2000/svg',
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedSetAttribute : SetAttributeNS = {
            key: 'href',
            value: 'google.com',
            namespace: 'http://www.w3.org/1999/xlink',
            type : "setAttributeNS",
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expectedCreateElement, expectedSetAttribute, appendOperation ]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expectedCreateElement, runtime);
        patch (domContext, appendOperation, runtime);
        patch (domContext, expectedSetAttribute, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: doesn't get appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('svg');
        // expect(runtime.nodes[nodeId]['href']).toEqual('google.com'); // dmj --- hrmmm?
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
    });
    test('Should process the RemoveAttribute patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        const tree : VNode<NodeId> = vnode({ tag: 'p', props: { foo: 'bar' }});
        const newTree : VNode<NodeId> = vnode({ tag: 'p' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expectedCreateElement : CreateElement = {
            tag: 'p',
            type : "createElement",
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedSetAttribute : SetAttribute = {
            key: 'foo',
            value: 'bar',
            type : "setAttribute",
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expectedCreateElement, expectedSetAttribute, appendOperation ]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expectedCreateElement, runtime);
        patch (domContext, appendOperation, runtime);
        patch (domContext, expectedSetAttribute, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: doesn't get appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('P');
        expect(runtime.nodes[nodeId].getAttribute('foo')).toEqual('bar');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
        let expectedRemoveAttribute : RemoveAttribute = {
            key: 'foo',
            type : "removeAttribute",
            nodeId,
        };
        diff (tree, newTree, parent, patchContext);
        expect(getPatches()).toEqual([expectedCreateElement, expectedSetAttribute, appendOperation, expectedRemoveAttribute]);
        patch (domContext, expectedRemoveAttribute, runtime);
        expect(runtime.nodes[nodeId].getAttribute('foo')).toEqual(null);
    });
    test('Should process the SetTextContent patch', () => {
        const nodeId : number = 1;
        const parentNodeId : number = 0;
        let tree : VText<NodeId> = vtext('foo');
        let newTree : VText<NodeId> = vtext('bar');
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        diff (tree, newTree, parent, patchContext);
        let expected : CreateTextNode = {
            text : 'foo',
            type : "createTextNode",
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedSetTextContent : SetTextContent = {
            text : 'bar',
            type : "setTextContent",
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation, expectedSetTextContent ]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(0); //dmj: doesn't get appended
        expect(runtime.nodes[nodeId].textContent).toEqual('foo');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);

        patch (domContext, appendOperation, runtime);
        patch (domContext, expectedSetTextContent, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(runtime.nodes[nodeId].textContent).toEqual('bar');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
    });

    test('Should process the AppendChild patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const nodeIdChild : number = 2;
        let tree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'a' })]});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            nodeId,
        };
        let appendOperation1 : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedChild : CreateElement = {
            type : "createElement",
            tag: 'a',
            nodeId: nodeIdChild,
        };
        let appendOperation2 : AppendChild = {
            type : "appendChild",
            parent: nodeId,
            child: nodeIdChild,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, expectedChild, appendOperation2, appendOperation1 ]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        patch (domContext, appendOperation1, runtime);
        patch (domContext, expectedChild, runtime);
        patch (domContext, appendOperation2, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('P');
        expect(runtime.nodes[nodeId].childNodes[0].nodeName).toEqual('A');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(runtime.nodes[nodeIdChild]['nodeId']).toEqual(nodeIdChild);
    });

    test('Should process the SetInlineStyle patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        let tree : VNode<NodeId> = vnode({ tag: 'p', css : { 'backgroundColor': 'red' }});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedStyle : SetInlineStyle = {
            type : "setInlineStyle",
            new : { 'backgroundColor' : 'red' },
            current : {},
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, expectedStyle, appendOperation ]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        patch (domContext, appendOperation, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('P');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);

        // dmj: check that the style gets applied
        patch (domContext, expectedStyle, runtime);
        expect(runtime.nodes[nodeId].style['backgroundColor']).toEqual('red');
    });
    test('Should process the Flush patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        let tree : VText<NodeId> = vtext('foo');
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, tree, parent, patchContext);
        let expected : CreateTextNode = {
            text : 'foo',
            type : "createTextNode",
            nodeId,
        };
        let appendOperation : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };

        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, appendOperation]);
        patchContext.flush ();
        expect(getPatches().length).toEqual(0);
    });

     test('Should process the ReplaceChild patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const nodeIdChild1 : number = 2;
        const nodeIdChild2 : number = 3;
        let firstTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'a' })]});
        let secondTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'img' })]});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, firstTree, parent, patchContext);
        diff (firstTree, secondTree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            nodeId,
        };
        let appendOperation1 : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedChild : CreateElement = {
            type : "createElement",
            tag: 'a',
            nodeId: nodeIdChild1,
        };
        let appendOperation2 : AppendChild = {
            type : "appendChild",
            parent: nodeId,
            child: nodeIdChild1,
        };
        let newNode : CreateElement = {
            tag: 'img',
            type : "createElement",
            nodeId: nodeIdChild2,
        };
        let replaceOp : ReplaceChild = {
            type : "replaceChild",
            parent: nodeId,
            current: nodeIdChild1,
            new: nodeIdChild2,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, expectedChild, appendOperation2, appendOperation1, newNode, replaceOp ]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        patch (domContext, appendOperation1, runtime);
        patch (domContext, expectedChild, runtime);
        patch (domContext, appendOperation2, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('P');
        expect(runtime.nodes[nodeId].childNodes[0].nodeName).toEqual('A');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(runtime.nodes[nodeIdChild1].nodeName).toEqual('A');
        expect(runtime.nodes[nodeIdChild1]['nodeId']).toEqual(nodeIdChild1);

        patch (domContext, newNode, runtime);
        patch (domContext, replaceOp, runtime);

        expect(runtime.nodes[nodeId].childNodes[0].nodeName).toEqual('IMG');
        expect(runtime.nodes[nodeIdChild2].nodeName).toEqual('IMG');
        expect(runtime.nodes[nodeIdChild1]).toEqual(undefined);
        expect(runtime.nodes[nodeIdChild2]['nodeId']).toEqual(nodeIdChild2);
        
    });

  test('Should process the RemoveChild patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const nodeIdChild : number = 2;
        let firstTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'a' })]});
        let secondTree : VNode<NodeId> = vnode({ tag: 'p' });
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, firstTree, parent, patchContext);
        diff (firstTree, secondTree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            nodeId,
        };
        let appendOperation1 : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedChild : CreateElement = {
            type : "createElement",
            tag: 'a',
            nodeId: nodeIdChild,
        };
        let appendOperation2 : AppendChild = {
            type : "appendChild",
            parent: nodeId,
            child: nodeIdChild,
        };
        let removeOp : RemoveChild = {
            type : "removeChild",
            child: nodeIdChild,
            parent: nodeId
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([expected, expectedChild, appendOperation2, appendOperation1, removeOp ]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        patch (domContext, appendOperation1, runtime);
        patch (domContext, expectedChild, runtime);
        patch (domContext, appendOperation2, runtime);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('P');
        expect(runtime.nodes[nodeId].childNodes[0].nodeName).toEqual('A');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(runtime.nodes[nodeId].childNodes.length).toEqual(1);
        expect(runtime.nodes[nodeIdChild].nodeName).toEqual('A');
        expect(runtime.nodes[nodeIdChild]['nodeId']).toEqual(nodeIdChild);

        patch (domContext, removeOp, runtime);
        expect(runtime.nodes[nodeId].childNodes.length).toEqual(0);
        expect(runtime.nodes[nodeIdChild]).toEqual(undefined);

    });


  test('Should process the InsertBefore patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        const nodeIdChild1 : number = 2;
        const nodeIdChild2 : number = 3;
        let firstTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'a', key: '1' }) ]});
        let secondTree : VNode<NodeId> = vnode({ tag: 'p', children: [vnode({ tag: 'img', key: '2' }) ]});
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, firstTree, parent, patchContext);
        diff (firstTree, secondTree, parent, patchContext);
        let expected : CreateElement = {
            tag: 'p',
            type : "createElement",
            nodeId,
        };
        let appendOperation1 : AppendChild = {
            type : "appendChild",
            parent: parentNodeId,
            child: nodeId,
        };
        let expectedChild1 : CreateElement = {
            type : "createElement",
            tag: 'a',
            nodeId: nodeIdChild1,
        };
        let appendOperation2 : AppendChild = {
            type : "appendChild",
            parent: nodeId,
            child: nodeIdChild1,
        };
        let expectedChild2 : CreateElement = {
            type : "createElement",
            tag: 'img',
            nodeId: nodeIdChild2,
        };
        let insertBeforeOp : InsertBefore = {
            type : "insertBefore",
            parent: nodeId,
            node: nodeIdChild2,
            child: nodeIdChild1,
        };
        let removeChildOp : RemoveChild = {
            type : "removeChild",
            child: nodeIdChild1,
            parent: nodeId
        };
        // dmj: check diff produces patch object
      expect(getPatches()).toEqual([expected, expectedChild1, appendOperation2, appendOperation1, expectedChild2, insertBeforeOp, removeChildOp ]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, expected, runtime);
        patch (domContext, appendOperation1, runtime);
        patch (domContext, expectedChild1, runtime);
        patch (domContext, appendOperation2, runtime);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes.length).toEqual(1); //dmj: appended
        expect(runtime.nodes[nodeId].nodeName).toEqual('P');
        expect(runtime.nodes[nodeId].childNodes[0].nodeName).toEqual('A');
        expect(runtime.nodes[nodeId]['nodeId']).toEqual(nodeId);
        expect(runtime.nodes[nodeId].childNodes.length).toEqual(1);
        expect(runtime.nodes[nodeIdChild1].nodeName).toEqual('A');
        expect(runtime.nodes[nodeIdChild1]['nodeId']).toEqual(nodeIdChild1);

        patch (domContext, expectedChild2, runtime);
        patch (domContext, insertBeforeOp, runtime);
        expect(runtime.nodes[nodeId].childNodes.length).toEqual(2);
        expect(document.body.firstChild.childNodes[1].previousSibling.nodeName).toEqual('IMG');

        patch (domContext, removeChildOp, runtime);
        expect(runtime.nodes[nodeIdChild1]).toEqual(undefined);
        expect(runtime.nodes[nodeIdChild2].nodeName).toEqual('IMG');
    });

  test('Should process the AddClass patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        let vtree = vnode({});
        vtree.classList = new Set(["foo", "bar"]);
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, vtree, parent, patchContext);
        let child : CreateElement = {
            type : "createElement",
            tag: 'div',
            nodeId,
        };
        let patch1 : AddClass = {
            key: 'foo',
            type : "addClass",
            nodeId,
        };
        let patch2 : AddClass = {
            key: 'bar',
            type : "addClass",
            nodeId,
        };
        let appendPatch : AppendChild = {
           child: 1,
           parent: 0,
           type: "appendChild",
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([child, patch1, patch2, appendPatch]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, child, runtime);
        patch (domContext, patch1, runtime);
        patch (domContext, patch2, runtime);
        patch (domContext, appendPatch, runtime);
        // dmj: check the DOM and runtime env. to see if the patch applied, and env updated
        expect(document.body.childNodes[0]['className']).toEqual("foo bar");
        expect(runtime.nodes[nodeId].nodeName).toEqual('DIV');
    });

  test('Should process the RemoveClass patch', () => {
        const parentNodeId : number = 0;
        const nodeId : number = 1;
        let vtree1 = vnode({});
        vtree1.classList = new Set(["foo", "bar"]);
        let vtree2 = vnode({});
        vtree2.classList = new Set(["foo"]);
        let patchContext : DrawingContext<NodeId> = patchDrawingContext;
        let domContext : DrawingContext<DOMRef> = drawingContext;
        let parent : NodeId = { nodeId : parentNodeId };
        diff (null, vtree1, parent, patchContext);
        diff (vtree1, vtree2, parent, patchContext);
        let child : CreateElement = {
            type : "createElement",
            tag: 'div',
            nodeId,
        };
        let patch1 : AddClass = {
            key: 'foo',
            type : "addClass",
            nodeId,
        };
        let patch2 : AddClass = {
            key: 'bar',
            type : "addClass",
            nodeId,
        };
        let appendPatch : AppendChild = {
           child: 1,
           parent: 0,
           type: "appendChild",
        };
        let patch3 : RemoveClass = {
            key: 'bar',
            type : "removeClass",
            nodeId,
        };
        // dmj: check diff produces patch object
        expect(getPatches()).toEqual([child, patch1, patch2, appendPatch, patch3]);
        let runtime : Runtime<DOMRef> = {
          nodes : { 0: document.body },
          components : {}
        };
        patch (domContext, child, runtime);
        patch (domContext, patch1, runtime);
        patch (domContext, patch2, runtime);
        patch (domContext, appendPatch, runtime);
        patch (domContext, patch3, runtime);
        // dmj: check the DOM and component env. to see if the patch applied, and env updated
        expect(document.body.childNodes[0]['className']).toEqual("foo");
        expect(runtime.nodes[nodeId].nodeName).toEqual('DIV');
    });


});
