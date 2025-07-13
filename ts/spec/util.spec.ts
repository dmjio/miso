import { callFocus, callBlur, getParentComponentId } from '../miso/util';
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

  test('Should call callFocus() and callBlur()', () => {
    var child = document.createElement('input');
    child['id'] = 'foo';
    document.body.appendChild(child);
    callFocus('blah', 0); /* missing case */
    callFocus('foo', 0); /* found case */
    callFocus('foo', 1); /* found case */
    expect(document.activeElement).toEqual(child);
    callBlur('blah', 0); /* missing case */
    callBlur('foo', 0); /* found case */
    callBlur('foo', 1); /* found case */
    expect(document.activeElement).toEqual(document.body);
  });

  test('Should get parentComponentId', () => {
    const grandparent = document.createElement('div');
    grandparent['componentId'] = 'grandparent';
    const parent = document.createElement('div');
    grandparent.appendChild(parent);
    const child = document.createElement('div');
    parent.appendChild(child);
    let vcomp = { 'domRef' : child };
    expect(getParentComponentId(vcomp)).toBe('grandparent');
    vcomp.domRef = parent;
    expect(getParentComponentId(vcomp)).toBe('grandparent');
    expect(getParentComponentId(grandparent)).toBe(null);
  });

});
