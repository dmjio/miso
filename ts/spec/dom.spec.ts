import { diff } from '../miso/dom';
import { vnode, vtext, vnodeKeyed, vtextKeyed } from '../miso/smart';
import { VNode, VTree } from '../miso/types';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';

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
    diff(null, null, document.body);
    expect(document.body.childNodes.length).toBe(0);
  });

  test('Should create a new text node', () => {
    var newNode = vtext('foo');
    diff(null, newNode, document.body);
    expect(newNode.domRef.textContent).toBe('foo');
  });

  test('Should window diff two identical text nodes', () => {
    var currentNode = vtext('foo');
    diff(null, currentNode, document.body);
    expect(currentNode.domRef.textContent).toBe('foo');
    var newNode = vtext('foo');
    diff(currentNode, newNode, document.body);
    expect('foo').toBe(newNode.domRef.textContent);
  });

  test('Should window diff two window different text nodes', () => {
    var currentNode = vtext('foo');
    diff(null, currentNode, document.body);
    expect(currentNode.domRef.textContent).toBe('foo');
    var newNode = vtext('bar');
    diff(currentNode, newNode, document.body);
    expect(newNode.domRef.textContent).toBe('bar');
  });

  test('Should create a new DOM node', () => {
    var newNode = vnode({});
    diff(null, newNode, document.body);
    expect(document.body.children[0]).toBe(newNode.domRef);
  });

  test('Should create an SVG DOM node', () => {
    var newNode = vnode({ ns: 'svg' });
    diff(null, newNode, document.body);
    expect(document.body.children[0]).toBe(newNode.domRef);
  });

  test('Should create a MathML DOM node', () => {
    var newNode = vnode({ ns: 'mathml', tag: 'math' });
    diff(null, newNode, document.body);
    expect(document.body.children[0].namespaceURI).toBe(
      'http://www.w3.org/1998/Math/MathML',
    );
  });

  test('Should create an SVG DOM node, with href attribute', () => {
    var tree = vnode({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        href: 'https://google.com',
      },
    });
    diff(null, tree, document.body);
    expect(
      document.body.children[0].getAttributeNS(
        'http://www.w3.org/1999/xlink',
        'href',
      ),
    ).toBe('https://google.com');
  });

  test('Should create an SVG DOM node, with href attribute, and change it', () => {
    var tree1 = vnode({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        href: 'https://google.com',
      },
    });
    diff(null, tree1, document.body);
    expect(
      document.body.children[0].getAttributeNS(
        'http://www.w3.org/1999/xlink',
        'href',
      ),
    ).toBe('https://google.com');
    var tree2 = vnode({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        href: 'https://yahoo.com',
      },
    });
    diff(tree1, tree2, document.body);
    expect(
      document.body.children[0].getAttributeNS(
        'http://www.w3.org/1999/xlink',
        'href',
      ),
    ).toBe('https://yahoo.com');
  });

  test('Should create an SVG DOM node, with regular attribute', () => {
    var tree = vnode({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        rx: '100',
      },
    });
    diff(null, tree, document.body);
    expect(document.body.children[0].getAttribute('rx')).toBe('100');
  });

  test('Should create an SVG DOM node, with regular attribute, and change it', () => {
    var tree1 = vnode({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        rx: '100',
      },
    });
    diff(null, tree1, document.body);
    expect(document.body.children[0].getAttribute('rx')).toBe('100');
    var tree2 = vnode({
      tag: 'ellipse',
      ns: 'svg',
      props: {
        rx: '200',
      },
    });
    diff(tree1, tree2, document.body);
    expect(document.body.children[0].getAttribute('rx')).toBe('200');
  });

  test('Should replace a Node with a new Node of a window different tag', () => {
    // populate DOM
    var tree1 = vnode({ tag: 'div' });
    diff(null, tree1, document.body);

    // Test node was populated
    expect(document.body.children.length).toBe(1);

    // Replace node
    var tree2 = vnode({ tag: 'a' });
    diff(tree1, tree2, document.body);

    // Test node is removed from DOM
    expect(document.body.children[0].tagName).toBe('A');
  });

  test('Should create children', () => {
    // populate DOM
    var tree = vnode({ children: [vnode({})] });
    diff(null, tree, document.body);
    expect(tree.domRef.children.length).toBe(1);
    expect(tree.children.length).toBe(1);
  });

  test('Should remove a child', () => {
    // populate DOM
    var tree1 = vnode({ children: [vnode({})] });
    diff(null, tree1, document.body);
    expect(tree1.domRef.children.length).toBe(1);

    // remove children from DOM
    var tree2 = vnode({ children: [] });
    diff(tree1, tree2, document.body);
    expect(tree2.domRef.childNodes.length).toBe(0);
  });

  test('Should replace Node with TextNode', () => {
    var node = vnode({});
    diff(null, node, document.body);
    expect(document.body.childNodes.length).toBe(1);
    var textNode = vtext('fooo');
    diff(node, textNode, document.body);
    expect(document.body.childNodes[0].textContent).toBe('fooo');
  });

  test('Should replace TextNode with Node', () => {
    // populate DOM
    var textNode = vtext('fooo');
    diff(null, textNode, document.body);

    // Test node was populated
    expect(document.body.childNodes.length).toBe(1);

    // Replace node
    var node = vnode({});
    diff(textNode, node, document.body);

    // Test node is removed from DOM
    expect(document.body.children[0].tagName).toBe('DIV');
  });

  test('Should remove a DOM node', () => {
    // populate DOM
    var newNode = vnode({});
    diff(null, newNode, document.body);

    // Test node was populated
    expect(document.body.children.length).toBe(1);

    // Remove node
    diff(newNode, null, document.body);

    // Test node is removed from DOM
    expect(document.body.children.length).toBe(0);
  });

  test('Should create a new property on a DOM node', () => {
    // populate DOM
    var currentNode = vnode({
      props: { id: 'a' },
    });
    diff(null, currentNode, document.body);
    expect(currentNode.domRef['id']).toBe('a');
  });

  test('Should skip if window diffing identical properties', () => {
    // populate DOM
    var currentNode = vnode({
      props: { id: 'a' },
    });
    diff(null, currentNode, document.body);

    var newNode = vnode({
      props: { id: 'a' },
    });
    diff(currentNode, newNode, document.body);
    expect(currentNode.domRef).toBe(newNode.domRef);
  });

  test('Should create a custom attribute on a DOM node', () => {
    // populate DOM
    var currentNode = vnode({
      props: {
        lol: 'lol',
      },
    });
    diff(null, currentNode, document.body);
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');
  });

  test('Should change a custom attribute on a DOM node', () => {
    // populate DOM
    var currentNode = vnode({
      props: {
        lol: 'lol',
      },
    });
    diff(null, currentNode, document.body);
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');

    var newNode = vnode({
      props: {
        lol: 'lolz',
      },
    });

    diff(currentNode, newNode, document.body);
    expect(currentNode.domRef.getAttribute('lol')).toBe('lolz');
  });

  test('Should remove a custom attribute from a DOM node', () => {
    // populate DOM
    var currentNode = vnode({
      props: {
        lol: 'lol',
      },
    });
    diff(null, currentNode, document.body);
    expect(currentNode.domRef.getAttribute('lol')).toBe('lol');

    // test property change
    var newNode = vnode({});
    diff(currentNode, newNode, document.body);
    expect(newNode.domRef.getAttribute('lol')).toBe(null);
  });

  test('Should remove a property from DOM node', () => {
    // populate DOM
    var currentNode = vnode({ props: { id: 'someid' } });
    diff(null, currentNode, document.body);
    expect(currentNode.domRef['id']).toBe('someid');

    // test property change
    var newNode = vnode({});
    diff(currentNode, newNode, document.body);
    expect(newNode.domRef['id']).toBe('');
  });

  test('Should change a property from DOM node', () => {
    // populate DOM
    var currentNode = vnode({ props: { id: 'someid' } });
    diff(null, currentNode, document.body);
    expect(currentNode.domRef['id']).toBe('someid');

    // test property change
    var newNode = vnode({ props: { id: 'foo' } });
    diff(currentNode, newNode, document.body);
    expect(newNode.domRef['id']).toBe('foo');
  });

  test('Should create css on a DOM node', () => {
    // populate DOM
    var newNode = vnode({
      css: {
        color: 'red',
      },
    });
    diff(null, newNode, document.body);
    expect(newNode.domRef.style['color']).toBe('red');
  });

  test('Should remove css from DOM node', () => {
    // populate DOM
    var currentNode = vnode({
      css: {
        color: 'red',
      },
    });
    diff(null, currentNode, document.body);

    // test css change
    var newNode = vnode({});
    diff(currentNode, newNode, document.body);
    expect(newNode.domRef.style['color']).toBe('');
  });

  test('Should change css on a DOM node', () => {
    // populate DOM
    var currentNode = vnode({
      css: {
        color: 'red',
      },
    });
    diff(null, currentNode, document.body);

    // test css change
    var newNode = vnode({
      css: {
        color: 'blue',
      },
    });
    diff(currentNode, newNode, document.body);
    expect(newNode.domRef.style['color']).toBe('blue');
  });

  test('Should no-op change to css on a DOM node', () => {
    // populate DOM
    var currentNode = vnode({
      css: {
        color: 'red',
      },
    });
    diff(null, currentNode, document.body);

    // test css no-op change
    var newNode = vnode({
      css: {
        color: 'red',
      },
    });
    diff(currentNode, newNode, document.body);
    expect(newNode.domRef.style['color']).toBe('red');
  });

  test('Should call onBeforeCreated', () => {
    // populate DOM
    let beforeCreated = 0;
    const currentNode = vnode({
      onBeforeCreated: () => {
        beforeCreated++;
      }
    });
    diff(null, currentNode, document.body);
    expect(beforeCreated).toBe(1);
  });

  test('Should call onCreated and onDestroyed', () => {
    // populate DOM
    let create = 0,
      destroy = 0;
    const currentNode = vnode({
      onCreated: () => {
        create++;
      },
      onDestroyed: () => {
        destroy++;
      },
    });

    diff(null, currentNode, document.body);
    expect(create).toBe(1);

    diff(currentNode, null, document.body);
    expect(destroy).toBe(1);
  });

  test('Should call onCreated and onBeforeDestroyed', () => {
    let create = 0,
      destroy = 0;
    const currentNode = vnode({
      onCreated: () => {
        create++;
      },
      onBeforeDestroyed: () => {
        destroy++;
      },
    });

    diff(null, currentNode, document.body);
    expect(create).toBe(1);

    diff(currentNode, null, document.body);
    expect(destroy).toBe(1);
  });

  test('Should call onDestroyed recursively', () => {
    let destroy = 0,
      childDestroy = 0;
    const child = vnode({
      onDestroyed: () => {
        childDestroy++;
      },
    });

    const parent = vnode({
      onDestroyed: () => {
        destroy++;
      },
      children: [child],
    });
    diff(null, parent, document.body);
    diff(parent, null, document.body);
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
  });

  test('Should call onBeforeDestroyed recursively', () => {
    var destroy = 0;
    var childDestroy = 0;

    const child = vnode({
      onDestroyed: () => {
        childDestroy++;
      }
    });

    const parent = vnode({
      onBeforeDestroyed: () => {
        destroy++;
      },
      children: [child],
    });
    diff(null, parent, document.body);
    diff(parent, null, document.body);
    expect(destroy).toBe(1);
    expect(childDestroy).toBe(1);
  });

  test('Should recreate a DOM node when tags are the same but keys are different', () => {
    var destroy = 0;
      var currentNode = vnode({
          onDestroyed : () => {
              destroy++;
          },
      });
    diff(null, currentNode, document.body);
      var newNode = vnode({
          onDestroyed : () => {
              destroy++;
          },
      });
    diff(null, currentNode, document.body);
    expect(destroy).toBe(0);
    diff(currentNode, newNode, document.body);
      var newKeyedNode = vnode({
          onDestroyed : () => {
              destroy++;
          },
          key : 'key-2'
      });
    diff(currentNode, newKeyedNode, document.body);
    expect(destroy).toBe(1);
  });

  test('Should execute left-hand side happy path key-window diffing case', () => {
    var body = document.body;
    var currentNode = vnode({
      children: [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'c'),
      ],
      key: 'key-1',
    });
    diff(null, currentNode, body);
    var newNode = vnode({
      children: [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'c'),
      ],
      key: 'key-1',
    });
    diff(currentNode, newNode, body);
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
    var currentNode = vnode({
      children : [vnodeKeyed('div', '1')],
    });
    diff(null, currentNode, body);
    var newNode = vnode({
        children : [vnodeKeyed('div', '2'), vnodeKeyed('div', '1')],
    });
    diff(currentNode, newNode, body);
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
    var currentNode = vnode({
        children : [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'c')],
    });
    diff(null, currentNode, body);
      var newNode = vnode({
          children : [vnodeKeyed('div', 'z'), vnodeKeyed('div', 'c')],
      });
    diff(currentNode, newNode, body);
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
    var currentNode = vnode({
        children : [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b')],
    });
    diff(null, currentNode, body);
    var newNode = vnode({
        children : [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'a')],
    });
    diff(currentNode, newNode, body);
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
    var currentNode = vnode({
        children : [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'c')],
    });
    diff(null, currentNode, body);
    var newNode = vnode({
        children : [vnodeKeyed('div', 'c'), vnodeKeyed('div', 'b'), vnodeKeyed('div', 'a')],
    });
    diff(currentNode, newNode, body);
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
    var kids : Array<VNode> = [];
    for (var i = 1; i < 1001; i++) kids.push(vnodeKeyed('div', i.toString()));
      var currentNode : any = vnode({
          children : kids
      });
    var newKids : Array<VTree> = [];
    for (i = 1; i < 1001; i++) {
      if (i == 3) {
        newKids.push(vnodeKeyed('div', '999'));
      } else if (i == 999) {
        newKids.push(vnodeKeyed('div', '3'));
      } else {
        newKids.push(vnodeKeyed('div', i.toString()));
      }
    }
    diff(null, currentNode, body);
      var newNode : any = vnode({
          children : newKids,
      });
    diff(currentNode, newNode, body);
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
      var currentNode = vnode({
          children : [
        vnodeKeyed('div', 'd'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'k'),
        vnodeKeyed('div', 'r'),
        vnodeKeyed('div', 'b'),
      ],
      });
    diff(null, currentNode, body);
    var newNode = vnode({
        children : [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'r'),
        vnodeKeyed('div', 'k'),
        vnodeKeyed('div', 'd'),
        ],
    });
    diff(currentNode, newNode, body);
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
      var currentNode = vnode({
          children : [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'b'),
          ],
      });
    diff(null, currentNode, body);
      var newNode = vnode({
          children : [
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'a'),
      ],
      });
    diff(currentNode, newNode, body);
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
      var currentNode = vnode({
          children : [
        vnodeKeyed('div', 'd'),
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'g'),
        vnodeKeyed('div', 'b'),
      ],
      });
    diff(null, currentNode, body);
      var newNode = vnode({
          children : [
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'g'),
        vnodeKeyed('div', 'd'),
        vnodeKeyed('div', 'a'),
      ],
      });
    diff(currentNode, newNode, body);
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
      var currentNode = vnode({
          children : [vnodeKeyed('div', 'e'), vnodeKeyed('div', 'k'), vnodeKeyed('div', 'l')],
      });
    diff(null, currentNode, body);
      var newNode = vnode({
          children : [vnodeKeyed('div', 'b'), vnodeKeyed('div', 'z'), vnodeKeyed('div', 'j')]
      });
    diff(currentNode, newNode, body);
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
      var currentNode = vnode({
          children : [
        vnodeKeyed('div', 'a'),
        vnodeKeyed('div', 'k'),
        vnodeKeyed('div', 'l'),
        vnodeKeyed('div', 'c'),
        vnodeKeyed('div', 'g'),
      ]
      });
    diff(null, currentNode, body);
      var newNode = vnode({
          children:
      [
        vnodeKeyed('div', 'b'),
        vnodeKeyed('div', 'c'),
        vnodeKeyed('div', 'l'),
        vnodeKeyed('div', 'r'),
        vnodeKeyed('div', 'k'),
      ]
      });
    diff(currentNode, newNode, body);
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
      var currentNode = vnode({
          children : [vnodeKeyed('div', 'a')]
      });
    diff(null, currentNode, document.body);
    var newNode = vnode({
        children : [vnodeKeyed('div', 'a'), vnodeKeyed('div', 'c'), vnodeKeyed('div', 'k')]
    });
    diff(currentNode, newNode, document.body);
    expect(newNode.children.length).toBe(3);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
      expect(newNode.children[i].key).toBe(currentNode.children[i].key);
      expect(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
    }
  });

  test('Should diff keyed text nodes', () => {
    var currentNode = vnode({
      tag: 'div',
      children: [
        vtextKeyed('foo','1'),
        vtextKeyed('bar','2'),
        vtextKeyed('baz','3'),
      ]
    });
    diff(null, currentNode, document.body);
    var newNode = vnode({
      tag: 'div',
      children: [
        vtextKeyed ('baz','3'),
        vtextKeyed ('baz','2'),
        vtextKeyed ('baz','1'),
      ]
    });
    diff(currentNode, newNode, document.body);
    expect(newNode.children.length).toBe(currentNode.children.length);
    for (var i = 0; i < newNode.children.length; i++) {
        expect(newNode.children[i].key).toBe(currentNode.children[i].key);
        expect(newNode.children[i].domRef).toBe(currentNode.children[i].domRef);
    }
  });

});
