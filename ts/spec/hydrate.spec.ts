/* imports */
import { hydrate, integrityCheck } from '../miso/iso';
import { vtree, vtext, vnodeKids } from '../miso/smart';
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
describe ("Hydration tests", () => {

  test('Should copy simple nested DOM into VTree', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var nestedDiv = document.createElement('div');
    div.appendChild(nestedDiv);
    var txt = document.createTextNode('foo');
    nestedDiv.appendChild(txt);
    var currentNode = vtree({
      children: [vtree({ children: [vtext('foo')] })],
    });
    hydrate(false, document.body, currentNode);
    expect(currentNode.children[0].children[0].text).toEqual('foo');
  });

  test('Should fail because of expecting text node', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var nestedDiv = document.createElement('div');
    div.appendChild(nestedDiv);
    var currentNode = vtree({ children: [vtext('foo')] });
    expect(hydrate(false, document.body, currentNode)).toEqual(false);
  });

  test('Should fail to hydrate because of expecting element', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var txt = document.createTextNode('foo');
    div.appendChild(txt);
    var currentNode = vtree({
      children: [vtree()],
    });
    expect(hydrate(false, document.body, currentNode)).toEqual(false);
  });

  test('Should fail to hydrate because of non-matching text', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var txt = document.createTextNode('foo');
    div.appendChild(txt);
    var currentNode = vtree({ children: [vtext('bar')] });
    expect(hydrate(false, document.body, currentNode)).toEqual(false);
  });

  test('Should fail to hydrate because of non-matching DOM and VDOM', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var txt = document.createTextNode('foobar');
    div.appendChild(txt);
    var currentNode = vtree({ children: [vtext('foo')] });
    expect(hydrate(false, document.body, currentNode)).toEqual(false);
  });

  test('Should copy DOM into VTree with multiple consecutive text nodes and collapse them', () => {
    var div = document.createElement('div');
    document.body.appendChild(div);
    var txt = document.createTextNode('foobarbaz');
    div.appendChild(txt);
    var currentNode = vtree({
      children: [vtext('foo'), vtext('bar'), vtext('baz')],
    });
    hydrate(false, document.body, currentNode);
    expect(div.childNodes[0].textContent).toEqual('foobarbaz');
  });

  test('Should copy DOM into VTree with multiple consecutive text nodes and collapse them without mount point', () => {
    const div = document.createElement('div');
    document.body.appendChild(div);
    const txt = document.createTextNode('foobarbaz');
    div.appendChild(txt);
    const currentNode = vtree({
      children: [
        vtext('foo'),
        vtext('bar'),
        vtext('baz'),
        vtree(),
        vtext('foo'),
        vtext('bar'),
        vtext('baz'),
      ],
    });
    hydrate(false, null, currentNode);
    // Expect "foobarbaz" to be split up into three nodes in the DOM
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
    var tree = vtree({ children: [vtree({ children: [vtext('foo')] })] });
    var succeeded = hydrate(false, misoDiv, tree);
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
    var currentNode = vnodeKids('div', [vnodeKids('div', [vtext('foo')])]);
    var succeeded = hydrate(true, document.body, currentNode);
    expect(currentNode.children[0].children[0].domRef.textContent).toEqual(
      'foo',
    );
    expect(succeeded).toEqual(false);
  });

  test('Should fail to mount on a text node', () => {
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(misoTxt);
    var tree = vtree({ children: [vtree({ children: [vtext('foo')] })] });
    expect(hydrate(true, misoTxt, tree)).toEqual(false);
  });

  test('Should not hydrate on an empty page', () => {
    var tree = vtree({ children: [vtree({ children: [vtext('foo')] })] });
    expect(hydrate(true, null, tree)).toEqual(false);
  });

  test('Should pass integrity check', () => {
    var body = document.body;
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({ children: [vtext('foo')] });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
  });

  test('Should fail integrity check on bad tag', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({ children: [vtext('foo')] });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.tag = 'lol';
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on bad tag in hydrate w/ logging enabled', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({ children: [vtext('fool')] });
    expect(hydrate(true, document.body, tree)).toEqual(false);
  });

  test('Should fail integrity check on differing vtext', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      children: [vtext('foo')],
    });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.children[0].text = 'oops';
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on differing child lengths', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      children: [vtext('foo')],
    });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.children = [];
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on differing styles', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    child.style['background-color'] = 'red';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      children: [{ type: 'vtext', text: 'foo' }],
      css: { 'background-color': 'red' },
    });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.css['background-color'] = 'green';
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on differing styles, for color', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    child.style['background-color'] = 'red';
    child.style['color'] = '#cccccc';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      children: [{ type: 'vtext', text: 'foo' }],
      css: { 'background-color': 'red', color: '#cccccc' },
    });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.css['color'] = '#dddddd';
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on differing props', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    child.style['background-color'] = 'red';
    child.className = 'something';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      props: { class: 'something' },
      children: [{ type: 'vtext', text: 'foo' }],
      css: { 'background-color': 'red' },
    });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.props['class'] = 'something-else';
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on differing height / width', () => {
    var child = document.createElement('img');
    var misoTxt = document.createTextNode('foo');
    child.style['background-color'] = 'red';
    child.className = 'something';
    child.height = 100;
    child.width = 100;
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      tag : 'img',
      props: { class: 'something', height: '100', width: '100' },
      children: [{ type: 'vtext', text: 'foo' }],
      css: { 'background-color': 'red' },
    });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.props['height'] = '200';
    tree.props['width'] = '200';
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on random property (title)', () => {
    var child = document.createElement('div');
    var misoTxt = document.createTextNode('foo');
    child['title'] = 'bar';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      props: { title: 'bar' },
      children: [vtext('foo')],
    });
    expect(hydrate(false, document.body, tree)).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.props['title'] = 'woz';
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on href', () => {
    var child = document.createElement('a');
    var misoTxt = document.createTextNode('foo');
    child.style['background-color'] = 'red';
    child.href = 'google.com';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      tag : 'a',
      props: { href: 'google.com' },
      children: [{ type: 'vtext', text: 'foo' }],
      css: { 'background-color': 'red' },
    });
    const result = hydrate(false, document.body, tree);
    expect(result).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.props['href'] = 'notgoogle.com';
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on vtext domRef', () => {
    var child = document.createElement('a');
    var misoTxt = document.createTextNode('foo');
    child.style['background-color'] = 'red';
    child.href = 'google.com';
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      tag : 'a',
      props: { href: 'google.com' },
      children: [vtext('foo')],
      css: { 'background-color': 'red' },
    });
    const result = hydrate(false, document.body, tree);
    expect(result).toEqual(true);
    expect(integrityCheck(tree)).toBe(true);
    tree.children[0].domRef = document.createElement('div');
    expect(integrityCheck(tree)).toBe(false);
  });

  test('Should fail integrity check on unknown property test', () => {
    var child = document.createElement('a');
    var misoTxt = document.createTextNode('foo');
    document.body.appendChild(child);
    child.appendChild(misoTxt);
    var tree = vtree({
      tag : 'a',
      props: { foobah: 'lol' },
      children: [vtext('foo')],
      ns: 'HTML',
    });
    const result = hydrate(false, document.body, tree);
    expect(result).toEqual(true);
    expect(integrityCheck(tree)).toBe(false);
  });

});
