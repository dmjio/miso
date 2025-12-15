/* imports */
import { hydrate, integrityCheck } from '../miso/hydrate';
import { vnode, vtext, vcomp, vnodeKids } from '../miso/smart';
import { VText, VNode, DOMRef, VComp } from '../miso/types';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { hydrationContext, drawingContext } from '../miso/context/dom';

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
describe ("Hydration tests", () => {

  test('Should copy simple nested DOM into VTree', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var nestedDiv = document.createElement('div');
    div.appendChild(nestedDiv);
    var txt = document.createTextNode('foo');
    nestedDiv.appendChild(txt);

    const currentNode : VNode<DOMRef> = vnode<DOMRef>({
      children: [vnode<DOMRef>({ children: [vtext('foo')] })],
    });
    const result = hydrate(false, document.body, currentNode, hydrationContext, drawingContext);
    expect(result).toBe(true);
    expect(currentNode.children[0].children[0].text).toEqual('foo');
  });

  test('Should fail because of expecting text node', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var nestedDiv = document.createElement('div');
    div.appendChild(nestedDiv);
    var currentNode = vnode<DOMRef>({ children: [vtext('foo')] });
    expect(hydrate(false, document.body, currentNode, hydrationContext,drawingContext)).toEqual(false);
  });

  test('Should fail to hydrate because of expecting element', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var txt = document.createTextNode('foo');
    div.appendChild(txt);
    var currentNode = vnode<DOMRef>({
      children: [vnode<DOMRef>({})],
    });
    expect(hydrate(false, document.body, currentNode, hydrationContext,drawingContext)).toEqual(false);
  });

  test('Should fail to hydrate because of non-matching text', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var txt = document.createTextNode('foo');
    div.appendChild(txt);
    var currentNode = vnode<DOMRef>({ children: [vtext('bar')] });
    expect(hydrate(false, document.body, currentNode, hydrationContext,drawingContext)).toEqual(false);
  });

  test('Should fail to hydrate because of non-matching DOM and VDOM', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var txt = document.createTextNode('foobar');
    div.appendChild(txt);
    var currentNode = vnode<DOMRef>({ children: [vtext('foo')] });
    expect(hydrate(false, document.body, currentNode, hydrationContext,drawingContext)).toEqual(false);
  });

  test('Should copy DOM into VTree with multiple consecutive text nodes and collapse them', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var txt = document.createTextNode('foobarbaz');
    div.appendChild(txt);
    var currentNode = vnode<DOMRef>({
      children: [vtext('foo'), vtext('bar'), vtext('baz')],
    });
    const result = hydrate(false, document.body, currentNode, hydrationContext, drawingContext);
    expect(result).toBe(true);
    expect(div.childNodes[0].textContent).toEqual('foobarbaz');
  });

  test('Should copy DOM into VTree with multiple consecutive text nodes and collapse them without mount point', () => {
    const div = document.createElement('div');
    document.body.appendChild(div);
    const txt1 = document.createTextNode('foobarbaz');
    div.appendChild(txt1);
    const mid = document.createElement('div');
    div.appendChild(mid);
    const txt2 = document.createTextNode('foobarbaz');
    div.appendChild(txt2);
    const currentNode = vnode<DOMRef>({
      children: [
        vtext('foo'),
        vtext('bar'),
        vtext('baz'),
        vnode<DOMRef>({}),
        vtext('foo'),
        vtext('bar'),
        vtext('baz'),
      ],
    });
    const result = hydrate(false, null, currentNode, hydrationContext, drawingContext);
    //Expect "foobarbaz" to be split up into three nodes in the DOM
    expect(result).toBe(true);
    expect(div.childNodes[0].textContent).toEqual('foobarbaz');
    expect(div.childNodes[2].textContent).toEqual('foobarbaz');
  });

  test('Should copy DOM into VTree at mountPoint', () => {
    var unrelatedDiv = document.createElement('div');
    document.body.appendChild(document.createElement('script'));
    document.body.appendChild(document.createTextNode('test'));
    document.body.appendChild(unrelatedDiv);
    var unrelatedTxt = document.createTextNode('Not part of Miso app');
    unrelatedDiv.appendChild(unrelatedTxt);
    var misoDiv = document.createElement('div');
    document.body.appendChild(misoDiv);
    var nestedDiv1 = document.createElement('div');
    misoDiv.appendChild(nestedDiv1);
    var nestedDiv2 = document.createElement('div');
    nestedDiv1.appendChild(nestedDiv2);
    var txt = document.createTextNode('foo');
    nestedDiv2.appendChild(txt);
    var tree:any = vnode<DOMRef>({ children: [vnode<DOMRef>({ children: [vtext('foo')] })] });
    var succeeded = hydrate(false, misoDiv, tree, hydrationContext, drawingContext);
    expect(tree.children[0].children[0].domRef).toEqual(txt);
    expect(succeeded).toEqual(true);
  });

  test('Should copy DOM into VTree at body w/ script / text siblings', () => {
    var unrelatedDiv = document.createElement('div');
    document.body.appendChild(document.createElement('script'));
    document.body.appendChild(document.createTextNode('test'));
    document.body.appendChild(unrelatedDiv);
    var unrelatedTxt = document.createTextNode('Not part of Miso app');
    unrelatedDiv.appendChild(unrelatedTxt);
    var misoDiv = document.createElement('div');
    document.body.appendChild(misoDiv);
    var nestedDiv1 = document.createElement('div');
    misoDiv.appendChild(nestedDiv1);
    var nestedDiv2 = document.createElement('div');
    nestedDiv1.appendChild(nestedDiv2);
    var txt = document.createTextNode('foo');
    nestedDiv2.appendChild(txt);
    var currentNode : any = vnodeKids('div', [vnodeKids('div', [vtext('foo')])]);
    var succeeded = hydrate(true, document.body, currentNode, hydrationContext, drawingContext);
    expect(currentNode.children[0].children[0].domRef.textContent).toEqual(
        new Text('foo').textContent
    );
    expect(succeeded).toEqual(false);
  });

  test('Should fail to mount on a text node', () => {
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(misoTxt);
    var tree = vnode<DOMRef>({ children: [vnode<DOMRef>({ children: [vtext('foo')] })] });
    expect(hydrate(true, misoTxt, tree, hydrationContext,drawingContext)).toEqual(false);
  });

  test('Should not hydrate on an empty page', () => {
    var tree = vnode<DOMRef>({ children: [vnode<DOMRef>({ children: [vtext('foo')] })] });
    expect(hydrate(true, null, tree, hydrationContext,drawingContext)).toEqual(false);
  });

  test('Should pass integrity check', () => {
    var body = document.body;
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({ children: [vtext('foo')] });
    expect(hydrate(false, document.body, tree, hydrationContext,drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
  });

  test('Should fail integrity check on bad tag', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({ children: [vtext('foo')] });
    expect(hydrate(false, document.body, tree, hydrationContext, drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    tree.tag = 'lol';
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on bad tag in hydrate w/ logging enabled', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({ children: [vtext('fool')] });
    expect(hydrate(true, document.body, tree, hydrationContext, drawingContext)).toEqual(false);
  });

  test('Should fail integrity check on differing vtext', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      children: [vtext('foo')],
    });
    expect(hydrate(false, document.body, tree, hydrationContext,drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    (tree.children[0] as VText<DOMRef>).text = 'oops';
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on differing child lengths', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      children: [vtext('foo')],
    });
    expect(hydrate(false, document.body, tree, hydrationContext,drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    tree.children = [];
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on differing styles', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    child.style['backgroundColor'] = 'red';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      children: [vtext('foo')],
      css: { 'backgroundColor': 'red' },
    });
    expect(hydrate(false, document.body, tree, hydrationContext,drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    tree.css['backgroundColor'] = 'green';
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on differing styles, for color', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    child.style['backgroundColor'] = 'red';
    child.style['color'] = '#cccccc';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      children: [vtext('foo')],
      css: { 'backgroundColor': 'red', color: '#cccccc' },
    });
    expect(hydrate(false, document.body, tree, hydrationContext,drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    tree.css['color'] = '#dddddd';
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on differing props', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    child.style['backgroundColor'] = 'red';
    child.className = 'something';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      props: { class: 'something' },
      children: [vtext('foo')],
      css: { 'backgroundColor': 'red' },
    });
    expect(hydrate(false, document.body, tree, hydrationContext,drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    tree.props['class'] = 'something-else';
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on differing height / width', () => {
    var child = document.createElement('img');
    var misoTxt = document.createTextNode('foo');
    child.style['backgroundColor'] = 'red';
    child.className = 'something';
    child.height = 100;
    child.width = 100;
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      tag : 'img',
      props: { class: 'something', height: '100', width: '100' },
      children: [vtext('foo')],
      css: { 'backgroundColor': 'red' },
    });
    expect(hydrate(false, document.body, tree, hydrationContext,drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    tree.props['height'] = '200';
    tree.props['width'] = '200';
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on random property (title)', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    child['title'] = 'bar';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      props: { title: 'bar' },
      children: [vtext('foo')],
    });
    expect(hydrate(false, document.body, tree, hydrationContext,drawingContext)).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    tree.props['title'] = 'woz';
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on href', () => {
    var child = document.createElement('a');
    var misoTxt = document.createTextNode('foo');
    child.style['backgroundColor'] = 'red';
    child.href = 'google.com';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      tag : 'a',
      props: { href: 'google.com' },
      children: [vtext('foo')],
      css: { 'backgroundColor': 'red' },
    });
    const result = hydrate(false, document.body, tree, hydrationContext, drawingContext);
    expect(result).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    tree.props['href'] = 'notgoogle.com';
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on vtext domRef', () => {
    var child = document.createElement('a');
    var misoTxt = document.createTextNode('foo');
    child.style['backgroundColor'] = 'red';
    child.href = 'google.com';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      tag : 'a',
      props: { href: 'google.com' },
      children: [vtext('foo')],
      css: { 'backgroundColor': 'red' },
    });
    const result = hydrate(false, document.body, tree, hydrationContext, drawingContext);
    expect(result).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
    (tree.children[0] as VNode<DOMRef>).domRef = document.createElement('div');
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should fail integrity check on unknown property test', () => {
    var child = document.createElement('a');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vnode<DOMRef>({
      tag : 'a',
      props: { foobah: 'lol' },
      children: [vtext('foo')],
    });
    const result = hydrate(false, document.body, tree, hydrationContext, drawingContext);
    expect(result).toEqual(true);
    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(false);
  });

  test('Should call mountComponent when hydrating VComp', () => {
    const txt = document.createTextNode('component content');
    document.body.appendChild(txt);

    let mountCalled = false;
    let componentId: any = null;
    let componentTree: any = null;

    const comp = vcomp<DOMRef>({
      key: 'test-comp',
      mount: (parent: any, callback: any) => {
        mountCalled = true;
        componentId = 1;
        componentTree = vtext<DOMRef>('component content');
        hydrate(false, parent, componentTree, hydrationContext, drawingContext);
        callback(componentId, componentTree);
      },
      unmount: () => {},
    }) as VComp<DOMRef>;

    const result = hydrate(false, document.body, comp, hydrationContext, drawingContext);

    expect(result).toBe(true);
    expect(mountCalled).toBe(true);
    expect(componentId).toBe(1);
    expect(comp.componentId).toBe(1);
    expect(comp.child).toBe(componentTree);
  });

  test('Should call mountComponent with onBeforeMounted hook', () => {
    const div = document.createElement('div');
    document.body.appendChild(div);

    let beforeMountedCalled = false;
    let onMountedCalled = false;

    const comp = vcomp<DOMRef>({
      key: 'test-comp-hooks',
      onBeforeMounted: () => {
        beforeMountedCalled = true;
      },
      onMounted: (domRef: any) => {
        onMountedCalled = true;
      },
      mount: (parent: any, callback: any) => {
        callback(2, vtext<DOMRef>('mounted component'));
      },
      unmount: () => {},
    }) as VComp<DOMRef>;

    const tree = vnode<DOMRef>({
      children: [comp],
    });

    const result = hydrate(false, document.body, tree, hydrationContext, drawingContext);

    expect(result).toBe(false); // Hydration fails, falls back to mounting
    expect(beforeMountedCalled).toBe(true);
    expect(onMountedCalled).toBe(true);
  });

  test('Should handle VComp with nested child VTree', () => {
    const div = document.createElement('div');
    document.body.appendChild(div);

    let childComponentTree: any = null;

    const comp = vcomp<DOMRef>({
      key: 'nested-comp',
      mount: (parent: any, callback: any) => {
        childComponentTree = vnode<DOMRef>({
          tag: 'span',
          children: [vtext<DOMRef>('nested child')],
        });
        callback(3, childComponentTree);
      },
      unmount: () => {},
    }) as VComp<DOMRef>;

    const tree = vnode<DOMRef>({
      children: [comp],
    });

    const result = hydrate(false, document.body, tree, hydrationContext, drawingContext);

    expect(result).toBe(false); // Hydration fails, falls back to mounting
    expect(comp.child).toBe(childComponentTree);
    expect((comp.child as VNode<DOMRef>).tag).toBe('span');
  });

  test('Should log warning when hydration fails with logLevel enabled', () => {
    const div = document.createElement('div');
    document.body.appendChild(div);

    // Create mismatched structure - DOM has div, VTree expects span
    const tree = vnode<DOMRef>({ tag: 'span', children: [vtext<DOMRef>('mismatch')] });

    const result = hydrate(true, document.body, tree, hydrationContext, drawingContext);

    expect(result).toBe(false);
  });

  test('Should log success when hydration succeeds with logLevel enabled', () => {
    const div = document.createElement('div');
    document.body.appendChild(div);
    const txt = document.createTextNode('test');
    div.appendChild(txt);

    const tree = vnode<DOMRef>({
      children: [vtext<DOMRef>('test')],
    });

    const result = hydrate(true, document.body, tree, hydrationContext, drawingContext);

    expect(result).toBe(true);
  });

  test('Should handle preamble with no mount point children after scripts', () => {
    // Create mount point with only script tags
    const mountDiv = document.createElement('div');
    const script1 = document.createElement('script');
    const script2 = document.createElement('script');
    mountDiv.appendChild(script1);
    mountDiv.appendChild(script2);
    document.body.appendChild(mountDiv);

    const tree = vnode<DOMRef>({
      children: [vtext<DOMRef>('content')],
    });

    const result = hydrate(false, mountDiv as any, tree, hydrationContext, drawingContext);

    // Should create a new div since only scripts are present
    expect(result).toBe(false);
  });

  test('Should parse RGB color correctly', () => {
    const div = document.createElement('div');
    div.style.color = 'rgb(255, 128, 64)';
    document.body.appendChild(div);

    const tree = vnode<DOMRef>({
      css: { color: 'rgb(255, 128, 64)' },
    });

    const hydrated = hydrate(false, document.body, tree, hydrationContext, drawingContext);
    expect(hydrated).toBe(true);      

    expect(integrityCheck(tree, hydrationContext, drawingContext)).toBe(true);
  });

  test('Should successfully walk and hydrate VComp in DOM', () => {
    const div = document.createElement('div');
    const span = document.createElement('span');
    div.appendChild(span);
    document.body.appendChild(div);

    const comp = vcomp<DOMRef>({
      key: 'walk-comp',
      mount: (parent: any, callback: any) => {
        callback(6, vnode<DOMRef>({ tag: 'span' }));
      },
      unmount: () => {},
    }) as VComp<DOMRef>;

    const tree = vnode<DOMRef>({
      children: [comp],
    });

    const result = hydrate(false, document.body, tree, hydrationContext, drawingContext);
    expect(result).toBe(true);
    expect(comp.componentId).toBe(6);
    expect(comp.child).toBeDefined();
  });

  test('Should handle VNode with VNode child in walk function', () => {
    const div = document.createElement('div');
    const child = document.createElement('span');
    div.appendChild(child);
    document.body.appendChild(div);

    const tree = vnode<DOMRef>({
      children: [vnode<DOMRef>({ tag: 'span' })],
    });

    const result = hydrate(false, document.body, tree, hydrationContext, drawingContext);

    expect(result).toBe(true);
    expect(tree.domRef).toBeDefined();
  });

});
