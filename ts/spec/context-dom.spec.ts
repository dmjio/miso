import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { eventContext, hydrationContext, componentContext, drawingContext } from '../miso/context/dom';
import { vnode, vcomp } from '../miso/smart';
import { DOMRef } from '../miso/types';
import { diff } from '../miso/dom';

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

describe('EventContext tests', () => {
  test('Should add event listener', () => {
    let clicked = false;
    const listener = () => { clicked = true; };
    const div = document.createElement('div');

    eventContext.addEventListener(div as DOMRef, 'click', listener, false);
    div.click();

    expect(clicked).toBe(true);
  });

  test('Should remove event listener', () => {
    let clickCount = 0;
    const listener = () => { clickCount++; };
    const div = document.createElement('div');

    eventContext.addEventListener(div as DOMRef, 'click', listener, false);
    div.click();
    expect(clickCount).toBe(1);

    eventContext.removeEventListener(div as DOMRef, 'click', listener, false);
    div.click();
    expect(clickCount).toBe(1); // Should not increment
  });

  test('Should check node equality', () => {
    const div1 = document.createElement('div');
    const div2 = document.createElement('div');

    expect(eventContext.isEqual(div1 as DOMRef, div1 as DOMRef)).toBe(true);
    expect(eventContext.isEqual(div1 as DOMRef, div2 as DOMRef)).toBe(false);
  });

  test('Should get event target', () => {
    const div = document.createElement('div');
    const event = new Event('click');
    Object.defineProperty(event, 'target', { value: div, enumerable: true });

    const target = eventContext.getTarget(event);
    expect(target).toBe(div);
  });

  test('Should get parent node', () => {
    const parent = document.createElement('div');
    const child = document.createElement('span');
    parent.appendChild(child);

    const result = eventContext.parentNode(child as DOMRef);
    expect(result).toBe(parent);
  });
});

describe('HydrationContext tests', () => {
  test('Should get inline style', () => {
    const div = document.createElement('div');
    div.style.color = 'red';

    const color = hydrationContext.getInlineStyle(div as DOMRef, 'color');
    expect(color).toBe('red');
  });

  test('Should get first child', () => {
    const parent = document.createElement('div');
    const child1 = document.createElement('span');
    const child2 = document.createElement('p');
    parent.appendChild(child1);
    parent.appendChild(child2);

    const first = hydrationContext.firstChild(parent as DOMRef);
    expect(first).toBe(child1);
  });

  test('Should get last child', () => {
    const parent = document.createElement('div');
    const child1 = document.createElement('span');
    const child2 = document.createElement('p');
    parent.appendChild(child1);
    parent.appendChild(child2);

    const last = hydrationContext.lastChild(parent as DOMRef);
    expect(last).toBe(child2);
  });

  test('Should get class attribute', () => {
    const div = document.createElement('div');
    div.className = 'foo bar';

    const className = hydrationContext.getAttribute(div as DOMRef, 'class');
    expect(className).toBe('foo bar');
  });

  test('Should get property attribute when key is in node', () => {
    const input = document.createElement('input');
    input.value = 'test';

    const value = hydrationContext.getAttribute(input as DOMRef, 'value');
    expect(value).toBe('test');
  });

  test('Should get attribute using getAttribute', () => {
    const div = document.createElement('div');
    div.setAttribute('data-test', 'value');

    const attr = hydrationContext.getAttribute(div as DOMRef, 'data-test');
    expect(attr).toBe('value');
  });

  test('Should get tag name', () => {
    const div = document.createElement('div');
    const tag = hydrationContext.getTag(div as DOMRef);
    expect(tag).toBe('DIV');
  });

  test('Should get text content', () => {
    const div = document.createElement('div');
    div.textContent = 'hello world';

    const text = hydrationContext.getTextContent(div as DOMRef);
    expect(text).toBe('hello world');
  });

  test('Should get children', () => {
    const parent = document.createElement('div');
    const child1 = document.createElement('span');
    const child2 = document.createElement('p');
    parent.appendChild(child1);
    parent.appendChild(child2);

    const children = hydrationContext.children(parent as DOMRef);
    expect(children.length).toBe(2);
    expect(children[0]).toBe(child1);
    expect(children[1]).toBe(child2);
  });
});

describe('ComponentContext tests', () => {
  test('Should call mountComponent without errors', () => {
    expect(() => {
      componentContext.mountComponent([], 1, {});
    }).not.toThrow();
  });

  test('Should call unmountComponent without errors', () => {
    expect(() => {
      componentContext.unmountComponent(1);
    }).not.toThrow();
  });

  test('Should call modelHydration without errors', () => {
    expect(() => {
      componentContext.modelHydration({});
    }).not.toThrow();
  });
});

describe('DrawingContext tests', () => {
  test('Should get next sibling for VNode', () => {
    const node1 = vnode<DOMRef>({ tag: 'div' });
    const node2 = vnode<DOMRef>({ tag: 'span' });
    node1.nextSibling = node2;
    const p = vnode<DOMRef>({ tag: 'span', children: [ node1, node2] });
    diff (null, p, document.body, drawingContext);
    expect(drawingContext.nextSibling(node1)).toBe(node2.domRef);
  });

  test('Should get next sibling for VComp by drilling', () => {
    const comp = vcomp<DOMRef>({ tag: 'div', mount : (p, cb) => {
      const node1 = vnode<DOMRef>({ tag: 'div' });
      diff (null, node1, p, drawingContext);
      cb (0, node1);
    }});

    const node1 = vnode<DOMRef>({ tag: 'div' });
    comp.nextSibling = node1;
    let p = vnode<DOMRef>({ tag: 'span', children: [ comp, node1 ] });
    diff (null, p, document.body, drawingContext);
    expect(drawingContext.nextSibling(comp)).toBe(node1.domRef);

    node1.nextSibling = comp;
    p = vnode<DOMRef>({ tag: 'span', children: [ node1, comp ] });
    diff (null, p, document.body, drawingContext);
    expect(drawingContext.nextSibling(node1)).toBe(comp.child.domRef);
  });

   test('Should return undefined when no next sibling', () => {
    const parent = document.createElement('div');
    const only = document.createElement('div');
    parent.appendChild(only);

    const node = vnode<DOMRef>({ tag: 'div' });
    node.domRef = only as DOMRef;

    const next = drawingContext.nextSibling(node);
    expect(next).toBeNull();
   });

  test('Should return undefined when no next sibling', () => {
    const comp = vcomp<DOMRef>({ tag: 'div', mount : (p, cb) => {
      const node1 = vnode<DOMRef>({ tag: 'div' });
      diff (null, node1, p, drawingContext);
      cb (0, node1);
    }});

    const node1 = vnode<DOMRef>({ tag: 'div' });
    // comp.nextSibling = node1; -- dmj: intentional
    let p = vnode<DOMRef>({ tag: 'span', children: [ comp, node1 ] });
    diff (null, p, document.body, drawingContext);

    expect(drawingContext.nextSibling(comp)).toBe(null);

    p = vnode<DOMRef>({ tag: 'span', children: [ node1, comp ] });
    diff (null, p, document.body, drawingContext);

    expect(drawingContext.nextSibling(comp)).toBe(null);
  });

  test('Should create text node', () => {
    const textNode = drawingContext.createTextNode('hello');
    expect(textNode.textContent).toBe('hello');
    expect(textNode.nodeType).toBe(3); // TEXT_NODE
  });

  test('Should create element with namespace', () => {
    const svgElement = drawingContext.createElementNS('http://www.w3.org/2000/svg', 'circle');
    expect(svgElement.namespaceURI).toBe('http://www.w3.org/2000/svg');
    expect(svgElement.tagName).toBe('circle');
  });

  test('Should append child', () => {
    const parent = document.createElement('div');
    const child = document.createElement('span');

    drawingContext.appendChild(parent as DOMRef, child as DOMRef);
    expect(parent.children.length).toBe(1);
    expect(parent.children[0]).toBe(child);
  });

  test('Should replace child', () => {
    const parent = document.createElement('div');
    const oldChild = document.createElement('span');
    const newChild = document.createElement('p');
    parent.appendChild(oldChild);

    drawingContext.replaceChild(parent as DOMRef, newChild as DOMRef, oldChild as DOMRef);
    expect(parent.children.length).toBe(1);
    expect(parent.children[0]).toBe(newChild);
  });

  test('Should remove child', () => {
    const parent = document.createElement('div');
    const child = document.createElement('span');
    parent.appendChild(child);

    drawingContext.removeChild(parent as DOMRef, child as DOMRef);
    expect(parent.children.length).toBe(0);
  });

  test('Should create element', () => {
    const div = drawingContext.createElement('div');
    expect(div.tagName).toBe('DIV');
  });

  test('Should add class', () => {
    const div = document.createElement('div');
    drawingContext.addClass('foo', div as DOMRef);
    expect(div.classList.contains('foo')).toBe(true);
  });

  test('Should not add empty class', () => {
    const div = document.createElement('div');
    drawingContext.addClass('', div as DOMRef);
    expect(div.classList.length).toBe(0);
  });

  test('Should remove class', () => {
    const div = document.createElement('div');
    div.classList.add('foo');

    drawingContext.removeClass('foo', div as DOMRef);
    expect(div.classList.contains('foo')).toBe(false);
  });

  test('Should not remove empty class', () => {
    const div = document.createElement('div');
    div.classList.add('foo');

    drawingContext.removeClass('', div as DOMRef);
    expect(div.classList.contains('foo')).toBe(true);
  });

  test('Should insert before', () => {
    const parent = document.createElement('div');
    const existing = document.createElement('span');
    const newNode = document.createElement('p');
    parent.appendChild(existing);

    drawingContext.insertBefore(parent as DOMRef, newNode as DOMRef, existing as DOMRef);
    expect(parent.children.length).toBe(2);
    expect(parent.children[0]).toBe(newNode);
    expect(parent.children[1]).toBe(existing);
  });

  test('Should swap DOM refs', () => {
    const parent = document.createElement('div');
    const a = document.createElement('span');
    const b = document.createElement('p');
    const c = document.createElement('div');
    parent.appendChild(a);
    parent.appendChild(b);
    parent.appendChild(c);

    // Just verify the function executes without error
    expect(() => {
      drawingContext.swapDOMRefs(a as DOMRef, b as DOMRef, parent as DOMRef);
    }).not.toThrow();

    // Verify all children are still in parent
    expect(parent.children.length).toBe(3);
  });

  test('Should set inline style - remove style when not in new CSS', () => {
    const div = document.createElement('div');
    div.style.color = 'red';
    div.style.fontSize = '12px';

    const currentCss = { color: 'red', fontSize: '12px' };
    const newCss = { color: 'blue' };

    drawingContext.setInlineStyle(currentCss, newCss, div as DOMRef);
    expect(div.style.color).toBe('blue');
    expect(div.style.fontSize).toBe('');
  });

  test('Should set inline style - update existing style', () => {
    const div = document.createElement('div');
    div.style.color = 'red';

    const currentCss = { color: 'red' };
    const newCss = { color: 'blue' };

    drawingContext.setInlineStyle(currentCss, newCss, div as DOMRef);
    expect(div.style.color).toBe('blue');
  });

  test('Should set inline style - add new styles', () => {
    const div = document.createElement('div');

    const currentCss = {};
    const newCss = { color: 'red', fontSize: '14px' };

    drawingContext.setInlineStyle(currentCss, newCss, div as DOMRef);
    expect(div.style.color).toBe('red');
    expect(div.style.fontSize).toBe('14px');
  });

  test('Should set inline style - using setProperty for custom properties', () => {
    const div = document.createElement('div');

    const currentCss = {};
    const newCss = { '--custom-color': 'blue' };

    drawingContext.setInlineStyle(currentCss, newCss, div as DOMRef);
    expect(div.style.getPropertyValue('--custom-color')).toBe('blue');
  });

  test('Should set inline style - remove custom property', () => {
    const div = document.createElement('div');
    div.style.setProperty('--custom-color', 'red');

    const currentCss = { '--custom-color': 'red' };
    const newCss = {};

    drawingContext.setInlineStyle(currentCss, newCss, div as DOMRef);
    expect(div.style.getPropertyValue('--custom-color')).toBe('');
  });

  test('Should set inline style - update custom property', () => {
    const div = document.createElement('div');
    div.style.setProperty('--custom-color', 'red');

    const currentCss = { '--custom-color': 'red' };
    const newCss = { '--custom-color': 'blue' };

    drawingContext.setInlineStyle(currentCss, newCss, div as DOMRef);
    expect(div.style.getPropertyValue('--custom-color')).toBe('blue');
  });

  test('Should set attribute', () => {
    const div = document.createElement('div');
    drawingContext.setAttribute(div as DOMRef, 'data-test', 'value');
    expect(div.getAttribute('data-test')).toBe('value');
  });

  test('Should set attribute with namespace', () => {
    const svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
    drawingContext.setAttributeNS(
      svg as DOMRef,
      'http://www.w3.org/1999/xlink',
      'href',
      'test.svg'
    );
    expect(svg.getAttributeNS('http://www.w3.org/1999/xlink', 'href')).toBe('test.svg');
  });

  test('Should remove attribute', () => {
    const div = document.createElement('div');
    div.setAttribute('data-test', 'value');

    drawingContext.removeAttribute(div as DOMRef, 'data-test');
    expect(div.hasAttribute('data-test')).toBe(false);
  });

  test('Should set text content', () => {
    const div = document.createElement('div');
    drawingContext.setTextContent(div as DOMRef, 'hello');
    expect(div.textContent).toBe('hello');
  });

  test('Should call flush without errors', () => {
    expect(() => {
      drawingContext.flush();
    }).not.toThrow();
  });

  test('Should get root as document body', () => {
    const root = drawingContext.getRoot();
    expect(root).toBe(document.body);
  });
});
