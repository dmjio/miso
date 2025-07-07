/* imports */
import { diff } from '../miso/dom';
import { vnode, vcomp, vtext } from '../miso/smart';
import { VTree } from '../miso/types';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { context } from '../miso/context/dom';

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

/* tests */
describe ('Component tests', () => {
  test('Should unmount recursively in order', () => {
    var unmounts = [];
    var build = (name, children) => {
        return vcomp ({
          children: children,
          'component-id': name,
          unmount: () => {
            unmounts.push(name);
          }
      });
    };

    var tree: VTree = build('one', [build('two', [build('three', [])])]);
    diff(null, tree, document.body, context);
    diff(tree, null, document.body, context);
    expect(unmounts).toEqual(['one', 'two', 'three']);
  });
  test('Should mount and unmount a component', () => {
    var mountCount = 0;
    var unmountCount = 0;
    var newNode = vcomp({
      mount: (domRef, callback) => {
        mountCount++;
        var node = vcomp({});
        diff(null, node, document.body, context);
        callback(node);
      },
      unmount: () => {
        unmountCount++;
      },
      props: { id: 'vcomp-foo' },
      css: {
        'background-color': 'red',
      },
    });
    diff(null, newNode, document.body, context);
    expect(mountCount).toBe(1);
    expect(newNode.children.length).toBe(1);
    expect(newNode.domRef.children.length).toBe(1);
    expect(newNode.domRef.id).toBe('vcomp-foo');
    expect(newNode.domRef.style['background-color']).toBe('red');
    diff(newNode, null, document.body, context);
    expect(unmountCount).toBe(1);
  });
  test('Should Diff attrs of two Components', () => {
    // populate DOM
    var mountCount = 0;
    var compNode1 = vcomp({
      mount: () => {
        mountCount++;
      },
      'component-id': 'vcomp-foo',
      css: { 'background-color': 'red' },
    });
    diff(null, compNode1, document.body, context);
    expect(mountCount).toBe(1);

    // Test node was populated
    expect(document.body.childNodes.length).toBe(1);
    expect((document.body.childNodes[0] as HTMLElement).style['background-color']).toBe('red');

    // Replace node
    mountCount = 0;
    var compNode2 = vcomp({
      mount: () => {
        mountCount++;
      },
      'component-id': 'vcomp-foo',
      css: { 'background-color': 'green' },
    });
    diff(compNode1, compNode2, document.body, context);
    expect((document.body.childNodes[0] as HTMLElement).style['background-color']).toBe('green');
  });

  test('Should replace Component with Component', () => {
    // populate DOM
    var comp1 = vcomp({ key : 'a', tag: 'a' });
    // dmj: tag always 'div' for component, this is just for a test to ensure swap property
    diff(null, comp1, document.body, context);
    expect(document.body.firstChild.tagName).toBe('A');

    // Test node was populated
    expect(document.body.childNodes.length).toBe(1);

    // Replace node
    var mountCount = 0;
    var comp2 = vcomp({
     'component-id': 'vcomp-id',
      key : 'b',
      mount: () => {
        mountCount++;
      },
    });
    diff(comp1, comp2, document.body, context);

    // Node is removed from DOM, Component is on the DOM
    expect(document.body.childNodes.length).toBe(1);
    expect(document.body.firstChild.tagName).toBe('DIV');
    expect(mountCount).toBe(1);
  });

  test('Should replace Node with Component', () => {
    // populate DOM
    var node = vnode({ tag: 'a' });
    diff(null, node, document.body, context);
    expect(document.body.firstChild.tagName).toBe('A');

    // Test node was populated
    expect(document.body.childNodes.length).toBe(1);

    // Replace node
    var mountCount = 0;
    var compNode = vcomp({
     'component-id': 'vcomp-id',
      mount: () => {
        mountCount++;
      },
    });
    diff(node, compNode, document.body, context);

    // Node is removed from DOM, Component is on the DOM
    expect(mountCount).toBe(1);
    expect(document.body.firstChild.tagName).toBe('DIV');
  });

  test('Should replace Text with Component', () => {
    // populate DOM
    var node = vtext('foo');
    diff(null, node, document.body, context);

    // Test node was populated
    expect(node.domRef.textContent).toBe('foo');
    expect(document.body.firstChild.nodeType).toBe(Node.TEXT_NODE);
    // Replace node
    var mountCount = 0;
    var compNode = vcomp({
      'component-id': 'vcomp-id',
      mount: () => {
        mountCount++;
      },
    });
    diff(node, compNode, document.body, context);

    // Node is removed from DOM, Component is on the DOM
    expect(mountCount).toBe(1);
    expect(document.body.firstChild.nodeType).toBe(Node.ELEMENT_NODE);
  });
  test('Should replace Component with TextNode', () => {
    var mountCount = 0, unmountCount = 0;
    var component = vcomp({
      mount: () => {
        return mountCount++;
      },
      unmount: () => {
        return unmountCount++;
      },
    });
    diff(null, component, document.body, context);
    // Test component was populated
    expect(document.body.childNodes.length).toBe(1);
    expect(mountCount).toBe(1);
    expect(unmountCount).toBe(0);

    // Replace component
    var textNode = vtext('fooo');
    diff(component, textNode, document.body, context);

    // Test node is removed from DOM
    expect(document.body.childNodes[0].textContent).toBe('fooo');
    expect(unmountCount).toBe(1);
  });

  test('Should replace Component with Node', () => {
    // populate DOM
    var mountCount = 0,
      unmountCount = 0;
    var component = vcomp({
      mount: () => {
        mountCount++;
      },
      unmount: () => {
        unmountCount++;
      },
    });
    diff(null, component, document.body, context);

    // Test component was populated
    expect(document.childNodes.length).toBe(1);
    expect(mountCount).toBe(1);
    expect(unmountCount).toBe(0);

    // Replace component
    diff(component, vnode({}), document.body, context);

    // Test node is removed from DOM
    expect(document.body.children[0].tagName).toBe('DIV');
    expect(unmountCount).toBe(1);
  });

  test('Should replace Node with Component', () => {
    // populate DOM
    let node = vnode({});
    let mountCount = 0;
    let unmountCount = 0;

    let component = vcomp({
      mount: () => {
        mountCount++;
      },
      unmount: () => {
        unmountCount++;
      },
    });

    diff(null, node, document.body, context);

    // Test component was populated
    expect(document.childNodes.length).toBe(1);
    expect(mountCount).toBe(0);
    expect(unmountCount).toBe(0);

    // Replace component
    diff(node, component, document.body, context);

    // Test node is removed from DOM
    expect(document.body.children[0].tagName).toBe('DIV');
    expect(unmountCount).toBe(0);
    expect(mountCount).toBe(1);
  });


})
