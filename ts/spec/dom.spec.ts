import { diff } from '../miso/dom';
import { vnode, vcomp, vtext, vnodeKeyed, vtextKeyed } from '../miso/smart';
import { VNode, VTree, DOMRef } from '../miso/types';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { drawingContext } from '../miso/context/dom';

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

describe('DOM tests', () => {
  test('Should be null when diffing two null virtual DOMs', () => {
    diff<DOMRef>(null, null, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(0);
  });

  test('Should create a new text node', () => {
    var newNode = vtext<DOMRef>('foo');
    diff<DOMRef>(null, newNode, document.body, drawingContext);
    expect(newNode.domRef.textContent).toBe('foo');
  });

  test('Should window diff two identical text nodes', () => {
    var currentNode = vtext<DOMRef>('foo');
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(currentNode.domRef.textContent).toBe('foo');
    var newNode = vtext<DOMRef>('foo');
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect('foo').toBe(newNode.domRef.textContent);
  });

  test('Should window diff two window different text nodes', () => {
    var currentNode = vtext<DOMRef>('foo');
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(currentNode.domRef.textContent).toBe('foo');
    var newNode = vtext<DOMRef>('bar');
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.domRef.textContent).toBe('bar');
  });

  test('Should create a new DOM node', () => {
    var newNode = vnode<DOMRef>({});
    diff<DOMRef>(null, newNode, document.body, drawingContext);
    expect(document.body.children[0]).toBe(newNode.domRef);
  });

  test('Should create an SVG DOM node', () => {
    var newNode = vnode<DOMRef>({ ns: 'svg' });
    diff<DOMRef>(null, newNode, document.body, drawingContext);
    expect(document.body.children[0]).toBe(newNode.domRef);
  });

  test('Should create a MathML DOM node', () => {
    var newNode = vnode<DOMRef>({ ns: 'mathml', tag: 'math' });
    diff<DOMRef>(null, newNode, document.body, drawingContext);
    expect(document.body.children[0].namespaceURI).toBe(
      'http://www.w3.org/1998/Math/MathML',
    );
  });

  test('Should draw the canvas if we need to', () => {
    var drawCount = 0;
    var tree = vnode<DOMRef>({
      tag: 'canvas',
      draw: () => {
        drawCount++;
      },
      ns: 'html'
    });
    diff<DOMRef>(null, tree, document.body, drawingContext);
    expect(drawCount).toBe(1);
  });

  test('Should create an SVG DOM node, with href attribute', () => {
    var tree = vnode<DOMRef>({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        href: 'https://google.com',
      },
    });
    diff<DOMRef>(null, tree, document.body, drawingContext);
    expect(
      document.body.children[0].getAttributeNS(
        'http://www.w3.org/1999/xlink',
        'href',
      ),
    ).toBe('https://google.com');
  });

  test('Should create an SVG DOM node, with href attribute, and change it', () => {
    var tree1 = vnode<DOMRef>({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        href: 'https://google.com',
      },
    });
    diff<DOMRef>(null, tree1, document.body, drawingContext);
    expect(
      document.body.children[0].getAttributeNS(
        'http://www.w3.org/1999/xlink',
        'href',
      ),
    ).toBe('https://google.com');
    var tree2 = vnode<DOMRef>({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        href: 'https://yahoo.com',
      },
    });
    diff<DOMRef>(tree1, tree2, document.body, drawingContext);
    expect(
      document.body.children[0].getAttributeNS(
        'http://www.w3.org/1999/xlink',
        'href',
      ),
    ).toBe('https://yahoo.com');
  });

  test('Should create an SVG DOM node, with regular attribute', () => {
    var tree = vnode<DOMRef>({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        rx: '100',
      },
    });
    diff<DOMRef>(null, tree, document.body, drawingContext);
    expect(document.body.children[0].getAttribute('rx')).toBe('100');
  });

  test('Should create an SVG DOM node, with regular attribute, and change it', () => {
    var tree1 = vnode<DOMRef>({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        rx: '100',
      },
    });
    diff<DOMRef>(null, tree1, document.body, drawingContext);
    expect(document.body.children[0].getAttribute('rx')).toBe('100');
    var tree2 = vnode<DOMRef>({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        rx: '200',
      },
    });
    diff<DOMRef>(tree1, tree2, document.body, drawingContext);
    expect(document.body.children[0].getAttribute('rx')).toBe('200');
  });

  test('Should replace a Node with a new Node of a window different tag', () => {
    // populate DOM
    var tree1 = vnode<DOMRef>({ tag: 'div' });
    diff<DOMRef>(null, tree1, document.body, drawingContext);

    // Test node was populated
    expect(document.body.children.length).toBe(1);

    // Replace node
    var tree2 = vnode<DOMRef>({ tag: 'a' });
    diff<DOMRef>(tree1, tree2, document.body, drawingContext);

    // Test node is removed from DOM
    expect(document.body.children[0].tagName).toBe('A');
  });

  test('Should create children', () => {
    // populate DOM
    var tree = vnode<DOMRef>({ children: [vnode<DOMRef>({})] });
    diff<DOMRef>(null, tree, document.body, drawingContext);
    expect(tree.domRef.children.length).toBe(1);
    expect(tree.children.length).toBe(1);
  });

  test('Should remove a child', () => {
    // populate DOM
    var tree1 = vnode<DOMRef>({ children: [vnode<DOMRef>({})] });
    diff<DOMRef>(null, tree1, document.body, drawingContext);
    expect(tree1.domRef.children.length).toBe(1);

    // remove children from DOM
    var tree2 = vnode<DOMRef>({ children: [] });
    diff<DOMRef>(tree1, tree2, document.body, drawingContext);
    expect(tree2.domRef.childNodes.length).toBe(0);
  });

  test('Should replace Node with TextNode', () => {
    var node = vnode<DOMRef>({});
    diff<DOMRef>(null, node, document.body, drawingContext);
    expect(document.body.childNodes.length).toBe(1);
    var textNode = vtext<DOMRef>('fooo');
    diff<DOMRef>(node, textNode, document.body, drawingContext);
    expect(document.body.childNodes[0].textContent).toBe('fooo');
  });

  test('Should replace TextNode with Node', () => {
    // populate DOM
    var textNode = vtext<DOMRef>('fooo');
    diff<DOMRef>(null, textNode, document.body, drawingContext);

    // Test node was populated
    expect(document.body.childNodes.length).toBe(1);

    // Replace node
    var node = vnode<DOMRef>({});
    diff<DOMRef>(textNode, node, document.body, drawingContext);

    // Test node is removed from DOM
    expect(document.body.children[0].tagName).toBe('DIV');
  });

  test('Should remove a DOM node', () => {
    // populate DOM
    var newNode = vnode<DOMRef>({});
    diff<DOMRef>(null, newNode, document.body, drawingContext);

    // Test node was populated
    expect(document.body.children.length).toBe(1);

    // Remove node
    diff<DOMRef>(newNode, null, document.body, drawingContext);

    // Test node is removed from DOM
    expect(document.body.children.length).toBe(0);
  });

  test('Should create a new property on a DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({
      props: { id: 'a' },
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(currentNode.domRef['id']).toBe('a');
  });

  test('Should skip if window diffing identical properties', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({
      props: { id: 'a' },
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);

    var newNode = vnode<DOMRef>({
      props: { id: 'a' },
    });
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(currentNode.domRef).toBe(newNode.domRef);
  });

  test('Should create a custom attribute on a DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({
      props: {
        lol: 'lol2',
      },
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol2');
  });

  test('Should change a custom attribute on a DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({
      props: {
        lol: 'lol2',
      },
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol2');

    var newNode = vnode<DOMRef>({
      props: {
        lol: 'lolz',
      },
    });

    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(currentNode.domRef.getAttribute('lol')).toBe('lolz');
  });

  test('Should remove a custom attribute from a DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({
      props: {
        lol: 'lol2',
      },
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol2');

    // test property change
    var newNode = vnode<DOMRef>({});
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.domRef.getAttribute('lol')).toBe(null);
  });

  test('Should remove a property from DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({ props: { id: 'someid' } });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(currentNode.domRef['id']).toBe('someid');

    // test property change
    var newNode = vnode<DOMRef>({});
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.domRef['id']).toBe('');
  });

  test('Should change a property from DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({ props: { id: 'someid' } });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(currentNode.domRef['id']).toBe('someid');

    // test property change
    var newNode = vnode<DOMRef>({ props: { id: 'foo' } });
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.domRef['id']).toBe('foo');
  });

  test('Should create css on a DOM node', () => {
    // populate DOM
    var newNode = vnode<DOMRef>({
      css: {
        color: 'red',
      },
    });
    diff<DOMRef>(null, newNode, document.body, drawingContext);
    expect(newNode.domRef.style['color']).toBe('red');
  });

  test('Should remove css from DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({
      css: {
        color: 'red',
      },
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);

    // test css change
    var newNode = vnode<DOMRef>({});
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.domRef.style['color']).toBe('');
  });

  test('Should change css on a DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({
      css: {
        color: 'red',
      },
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);

    // test css change
    var newNode = vnode<DOMRef>({
      css: {
        color: 'blue',
      },
    });
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.domRef.style['color']).toBe('blue');
  });

  test('Should no-op change to css on a DOM node', () => {
    // populate DOM
    var currentNode = vnode<DOMRef>({
      css: {
        color: 'red',
      },
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);

    // test css no-op change
    var newNode = vnode<DOMRef>({
      css: {
        color: 'red',
      },
    });
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.domRef.style['color']).toBe('red');
  });

  test('Should call onBeforeCreated', () => {
    // populate DOM
    let beforeCreated = 0;
    const currentNode = vnode<DOMRef>({
      onBeforeCreated: () => {
        beforeCreated++;
      }
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(beforeCreated).toBe(1);
  });

  test('Should call onCreated and onDestroyed', () => {
    // populate DOM
    let create = 0,
      destroy = 0;
    const currentNode = vnode<DOMRef>({
      onCreated: () => {
        create++;
      },
      onDestroyed: () => {
        destroy++;
      },
    });

    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(create).toBe(1);

    diff<DOMRef>(currentNode, null, document.body, drawingContext);
    expect(destroy).toBe(1);
  });

  test('Should call entire mounting lifecycle', () => {
    let beforeMounted = 0;
    let mount = 0;
    let mounted = 0;
    let beforeUnmounted = 0;
    let unmounted = 0;
    let unmount = 0;
    const currentNode = vcomp<DOMRef>({
      onBeforeMounted: () => {
        beforeMounted++;
      },
      mount: () => {
        mount++;
        mounted++; //dmj : 'onMounted' is called inside of 'mount' callback in dom.ts
      },
      onBeforeUnmounted: () => {
        beforeUnmounted++;
      },
      onUnmounted: () => {
        unmounted++;
      },
      unmount: () => {
        unmount++;
      }
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(beforeMounted).toBe(1);
    expect(mount).toBe(1);
    expect(mounted).toBe(1);

    diff<DOMRef>(currentNode, null, document.body, drawingContext);
    expect(beforeUnmounted).toBe(1);
    expect(unmounted).toBe(1);
    expect(unmount).toBe(1);
    expect(mounted).toBe(1);
  });


  test('Should call onCreated and onBeforeDestroyed', () => {
    let create = 0,
      destroy = 0;
    const currentNode = vnode<DOMRef>({
      onCreated: () => {
        create++;
      },
      onBeforeDestroyed: () => {
        destroy++;
      },
    });

    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(create).toBe(1);

    diff<DOMRef>(currentNode, null, document.body, drawingContext);
    expect(destroy).toBe(1);
  });

  test('Should call onDestroyed recursively', () => {
    let destroy = 0,
      childDestroy = 0;
    const child = vnode<DOMRef>({
      onDestroyed: () => {
        childDestroy++;
      },
    });

    const parent = vnode<DOMRef>({
      onDestroyed: () => {
        destroy++;
      },
      children: [child],
    });
    diff<DOMRef>(null, parent, document.body, drawingContext);
    diff<DOMRef>(parent, null, document.body, drawingContext);
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
  });

  test('Should call onBeforeDestroyed recursively', () => {
    var destroy = 0;
    var childDestroy = 0;

    const child = vnode<DOMRef>({
      onDestroyed: () => {
        childDestroy++;
      }
    });

    const parent = vnode<DOMRef>({
      onBeforeDestroyed: () => {
        destroy++;
      },
      children: [child],
    });
    diff<DOMRef>(null, parent, document.body, drawingContext);
    diff<DOMRef>(parent, null, document.body, drawingContext);
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
  });

  test('Should recreate a DOM node when tags are the same but keys are different', () => {
    var destroy = 0;
      var currentNode = vnode<DOMRef>({
          onDestroyed : () => {
              destroy++;
          },
      });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
      var newNode = vnode<DOMRef>({
          onDestroyed : () => {
              destroy++;
          },
      });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    expect(destroy).toBe(0);
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
      var newKeyedNode = vnode<DOMRef>({
          onDestroyed : () => {
              destroy++;
          },
          key : 'key-2'
      });
    diff<DOMRef>(currentNode, newKeyedNode, document.body, drawingContext);
    expect(destroy).toBe(1);
  });

  test('Should execute left-hand side happy path key-window diffing case', () => {
    var body = document.body;
    var currentNode = vnode<DOMRef>({
      children: [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'c'),
      ],
      key: 'key-1',
    });
    diff<DOMRef>(null, currentNode, body, drawingContext);
    var newNode = vnode<DOMRef>({
      children: [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'c'),
      ],
      key: 'key-1',
    });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
      expect(newNode.children[i].key).toBe(currentNode.children[i].key);
      expect(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should diff keys properly when keys are prepended', () => {
    var body = document.body;
    var currentNode = vnode<DOMRef>({
      children : [vnodeKeyed('div', '1')],
    });
    diff<DOMRef>(null, currentNode, body, drawingContext);
    var newNode = vnode<DOMRef>({
        children : [vnodeKeyed('div', '2'), vnodeKeyed('div', '1')],
    });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should execute right-hand side happy path key-window diffing case', () => {
    var body = document.body;
    var currentNode = vnode<DOMRef>({
        children : [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'c')],
    });
    diff<DOMRef>(null, currentNode, body, drawingContext);
      var newNode = vnode<DOMRef>({
          children : [vnodeKeyed('div', 'z'), vnodeKeyed('div', 'c')],
      });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should swap nodes', () => {
    var body = document.body;
    var currentNode = vnode<DOMRef>({
        children : [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b')],
    });
    diff<DOMRef>(null, currentNode, body, drawingContext);
    var newNode = vnode<DOMRef>({
        children : [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'a')],
    });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(2);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should execute flip-flop case', () => {
    var body = document.body;
    var currentNode = vnode<DOMRef>({
        children : [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c')],
    });
    diff<DOMRef>(null, currentNode, body, drawingContext);
    var newNode = vnode<DOMRef>({
        children : [vnodeKeyed('div', 'c'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'a')],
    });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    for (var i = 0; i < 3; i++) {
      expect(currentNode.children[i]).not.toBe(undefined);
      expect(newNode.children[i]).not.toBe(undefined);
    }
  });

  test('Should execute swapped case on 1k nodes', () => {
    var body = document.body;
    var kids : Array<VNode<DOMRef>> = [];
    for (var i = 1; i < 1001; i++) kids.push(vnodeKeyed('div', i.toString()));
      var currentNode : any = vnode<DOMRef>({
          children : kids
      });
    var newKids : Array<VTree<DOMRef>> = [];
    for (i = 1; i < 1001; i++) {
      if (i == 3) {
        newKids.push(vnodeKeyed('div', '999'));
      } else if (i == 999) {
        newKids.push(vnodeKeyed('div', '3'));
      } else {
        newKids.push(vnodeKeyed('div', i.toString()));
      }
    }
    diff<DOMRef>(null, currentNode, body, drawingContext);
      var newNode : any = vnode<DOMRef>({
          children : newKids,
      });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(1000);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
    for (i = 0; i < 1000; i++) {
      expect(newNode.children[i].key).toBe(currentNode.children[i].key);
      expect(newNode.children[i].children[0].text).toBe(
        currentNode.children[i].children[0].text,
      );
      expect(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
      expect(newNode.children[i].domRef).not.toBe(undefined);
      expect(currentNode.children[i].domRef).not.toBe(undefined);
    }
  });

  test('Should execute top-left and bottom-right match case', () => {
    var body = document.body;
      var currentNode = vnode<DOMRef>({
          children : [
        vnodeKeyed('div', 'd'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'k'),
        vnodeKeyed('div', 'r'),
        vnodeKeyed('div', 'b'),
      ],
      });
    diff<DOMRef>(null, currentNode, body, drawingContext);
    var newNode = vnode<DOMRef>({
        children : [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'r'),
        vnodeKeyed('div', 'k'),
        vnodeKeyed('div', 'd'),
        ],
    });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(5);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should handle duplicate keys case', () => {
    var body = document.body;
      var currentNode = vnode<DOMRef>({
          children : [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'b'),
          ],
      });
    diff<DOMRef>(null, currentNode, body, drawingContext);
      var newNode = vnode<DOMRef>({
          children : [
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'a'),
      ],
      });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(5);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should execute top-right and bottom-left match case', () => {
    var body = document.body;
      var currentNode = vnode<DOMRef>({
          children : [
        vnodeKeyed('div', 'd'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'g'),
        vnodeKeyed('div', 'b'),
      ],
      });
    diff<DOMRef>(null, currentNode, body, drawingContext);
      var newNode = vnode<DOMRef>({
          children : [
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'g'),
        vnodeKeyed('div', 'd'),
        vnodeKeyed('div', 'a'),
      ],
      });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(4);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should match nothing', () => {
    var body = document.body;
      var currentNode = vnode<DOMRef>({
          children : [vnodeKeyed('div', 'e'), vnodeKeyed('div', 'k'), vnodeKeyed('div', 'l')],
      });
    diff<DOMRef>(null, currentNode, body, drawingContext);
      var newNode = vnode<DOMRef>({
          children : [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'z'), vnodeKeyed('div', 'j')]
      });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should handle nothing matches case where new key is found in old map', () => {
    var body = document.body;
      var currentNode = vnode<DOMRef>({
          children : [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'k'),
        vnodeKeyed('div', 'l'),
        vnodeKeyed('div', 'c'),
        vnodeKeyed('div', 'g'),
      ]
      });
    diff<DOMRef>(null, currentNode, body, drawingContext);
      var newNode = vnode<DOMRef>({
          children:
      [
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'c'),
        vnodeKeyed('div', 'l'),
        vnodeKeyed('div', 'r'),
        vnodeKeyed('div', 'k'),
      ]
      });
    diff<DOMRef>(currentNode, newNode, body, drawingContext);
    expect(newNode.children.length).toBe(5);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(currentNode.children[i].key).toEqual(newNode.children[i].key);
        expect(currentNode.children[i].domRef).toEqual(newNode.children[i].domRef);
    }
    expect(currentNode.domRef.children).toEqual(newNode.domRef.children);
    expect(currentNode.domRef.childNodes).toEqual(newNode.domRef.childNodes);
  });

  test('Should append new nodes in keys patch', () => {
      var currentNode = vnode<DOMRef>({
          children : [vnodeKeyed('div', 'a')]
      });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    var newNode = vnode<DOMRef>({
        children : [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'c'), vnodeKeyed('div', 'k')]
    });
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
      expect(newNode.children[i].key).toBe(currentNode.children[i].key);
      expect(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
    }
  });

  test('Should diff keyed text nodes', () => {
    var currentNode = vnode<DOMRef>({
      tag: 'div',
      children: [
        vtextKeyed('foo','1'),
        vtextKeyed('bar','2'),
        vtextKeyed('baz','3'),
      ]
    });
    diff<DOMRef>(null, currentNode, document.body, drawingContext);
    var newNode = vnode<DOMRef>({
      tag: 'div',
      children: [
        vtextKeyed ('baz','3'),
        vtextKeyed ('baz','2'),
        vtextKeyed ('baz','1'),
      ]
    });
    diff<DOMRef>(currentNode, newNode, document.body, drawingContext);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(newNode.children[i].key).toBe(currentNode.children[i].key);
        expect(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
    }
  });

});
