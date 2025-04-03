/* imports */
import { diff } from '../miso/dom';
import { mkVTree, mkVComp, vtree, vcomp, vtext } from '../miso/smart';
import { VTree } from '../miso/types';
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

/* tests */
describe ('Component tests', () => {
  test('Should unmount recursively in order', () => {
    var unmounts = [];
    var build = (name, children) => {
        return vcomp ({
          children: children,
          'data-component-id': name,
          unmount: () => {
            unmounts.push(name);
          }
      });
    };

    var tree: VTree = build('one', [build('two', [build('three', [])])]);
    diff(null, tree, document.body);
    diff(tree, null, document.body);
    expect(unmounts).toEqual(['one', 'two', 'three']);
  });
  test('Should detect duplicate component mounting', () => {
    var mountCount = 0;
    var newComp1 = vtree({
      type: 'vcomp',
      mount: () => {
        mountCount++;
      },
      'data-component-id': 'vcomp-foo',
    });
    diff(null, newComp1, document.body);
    var newComp2 = vtree({
      type: 'vcomp',
      mount: () => {
        mountCount++;
      },
      'data-component-id': 'vcomp-foo',
    });
    var newNode = vtree({ children: [newComp2] });
    diff(null, newNode, document.body);
    expect(mountCount).toBe(1);
  });

  test('Should mount and unmount a component', () => {
    var mountCount = 0;
    var unmountCount = 0;
    var newNode = vtree({
      type: 'vcomp',
      mount: (cb) => {
        mountCount++;
        var node = mkVComp();
        diff(null, node, document.body);
        cb(node);
      },
      unmount: () => {
        unmountCount++;
      },
      props: { id: 'vcomp-foo' },
      css: {
        'background-color': 'red',
      },
    });
    diff(null, newNode, document.body);
    expect(mountCount).toBe(1);
    expect(newNode.children.length).toBe(1);
    expect(newNode.domRef.children.length).toBe(1);
    expect(newNode.domRef.id).toBe('vcomp-foo');
    expect(newNode.domRef.style['background-color']).toBe('red');
    diff(newNode, null, document.body);
    expect(unmountCount).toBe(1);
  });
  test('Should Diff attrs of two Components', () => {
    // populate DOM
    var mountCount = 0;
    var compNode1 = vtree({
      type: 'vcomp',
      mount: () => {
        mountCount++;
      },
      'data-component-id': 'vcomp-foo',
      css: { 'background-color': 'red' },
    });
    diff(null, compNode1, document.body);
    expect(mountCount).toBe(1);

    // Test node was populated
    expect(document.body.childNodes.length).toBe(1);
    expect((document.body.childNodes[0] as HTMLElement).style['background-color']).toBe('red');

    // Replace node
    mountCount = 0;
    var compNode2 = vtree({
      type: 'vcomp',
      mount: () => {
        mountCount++;
      },
      'data-component-id': 'vcomp-foo',
      css: { 'background-color': 'green' },
    });
    diff(compNode1, compNode2, document.body);
    expect((document.body.childNodes[0] as HTMLElement).style['background-color']).toBe('green');
  });

  test('Should replace Node with Component', () => {
    // populate DOM
    var node = mkVComp();
    diff(null, node, document.body);

    // Test node was populated
    expect(document.body.childNodes.length).toBe(1);

    // Replace node
    var mountCount = 0;
    var compNode = vtree({
      type: 'vcomp',
      'data-component-id': 'vcomp-id',
      mount: () => {
        mountCount++;
      },
    });
    diff(node, compNode, document.body);

    // Node is removed from DOM, Component is on the DOM
    expect((document.body.childNodes[0] as Element).getAttribute('data-component-id')).toBe(
      'vcomp-id',
    );
    expect(mountCount).toBe(1);
  });

  test('Should replace Text with Component', () => {
    // populate DOM
    var node = vtree({ type: 'vtext', 'text': 'foo' });
    diff(null, node, document.body);

    // Test node was populated
    expect(node.domRef.textContent).toBe('foo');
    expect(document.body.childNodes.length).toBe(1);

    // Replace node
    var mountCount = 0;
    var compNode = vtree({
      type: 'vcomp',
      'data-component-id': 'vcomp-id',
      mount: () => {
        mountCount++;
      },
    });
    diff(node, compNode, document.body);

    // Node is removed from DOM, Component is on the DOM
    expect((document.body.childNodes[0] as Element).getAttribute('data-component-id')).toBe(
      'vcomp-id',
    );
    expect(mountCount).toBe(1);
  });
  test('Should replace Component with TextNode', () => {
    var mountCount = 0, unmountCount = 0;
    var component = vtree({
      mount: () => {
        return mountCount++;
      },
      unmount: () => {
        return unmountCount++;
      },
      type: 'vcomp',
    });
    diff(null, component, document.body);
    // Test component was populated
    expect(document.body.childNodes.length).toBe(1);
    expect(mountCount).toBe(1);
    expect(unmountCount).toBe(0);

    // Replace component
    var textNode = vtext('fooo');
    diff(component, textNode, document.body);

    // Test node is removed from DOM
    expect(document.body.childNodes[0].textContent).toBe('fooo');
    expect(unmountCount).toBe(1);
  });

  test('Should replace Component with Node', () => {
    // populate DOM
    var mountCount = 0,
      unmountCount = 0;
    var component = vtree({
      type: 'vcomp',
      mount: () => {
        mountCount++;
      },
      unmount: () => {
        unmountCount++;
      },
    });
    diff(null, component, document.body);

    // Test component was populated
    expect(document.childNodes.length).toBe(1);
    expect(mountCount).toBe(1);
    expect(unmountCount).toBe(0);

    // Replace component
    diff(component, mkVComp(), document.body);

    // Test node is removed from DOM
    expect(document.body.children[0].tagName).toBe('DIV');
    expect(unmountCount).toBe(1);
  });


})
