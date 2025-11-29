import { callFocus, callBlur, callSelect, callSetSelectionRange, getParentComponentId } from '../miso/util';
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
describe ('Utils tests', () => {

  test('Should call callFocus(), callSelect(), callBlur() and callSetSelectionRange()', () => {
    var child = document.createElement('input');
    child['id'] = 'foo';
    child['value'] = 'bar';
    document.body.appendChild(child);
    callFocus('blah', 0); /* missing case */
    callFocus('foo', 0); /* found case */
    callFocus('foo', 1); /* found case */
    expect(document.activeElement).toEqual(child);
    callSelect('blah', 0);
    callSelect('foo', 0);
    callSelect('foo', 1);
    var e : HTMLInputElement = document.querySelector('#foo');
    expect(e.selectionStart).toEqual(0);
    expect(e.selectionEnd).toEqual(3);
    callSetSelectionRange('blah', 1, 2, 0);
    callSetSelectionRange('foo', 1, 2, 0);
    callSetSelectionRange('foo', 1, 2, 1)
    expect(e.selectionStart).toEqual(1);
    expect(e.selectionEnd).toEqual(2);
    callBlur('blah', 0); /* missing case */
    callBlur('foo', 0); /* found case */
    callBlur('foo', 1); /* found case */
    expect(document.activeElement).toEqual(document.body);
  });

  test('Should get parentComponentId', () => {
    /* build */
    const grandparent = document.createElement('div');
    grandparent['componentId'] = 100;
    const parent = document.createElement('div');
    grandparent.appendChild(parent);
    const child = document.createElement('div');
    parent.appendChild(child);
    /* test */
    let vcomp = child;
    expect(getParentComponentId(vcomp)).toBe(100);
    vcomp = parent;
    expect(getParentComponentId(vcomp)).toBe(100);
    expect(getParentComponentId(grandparent)).toBe(null);
  });

});
