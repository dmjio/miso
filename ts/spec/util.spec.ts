import { callFocus, callBlur } from '../miso/util';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { context } from '../miso/context/dom'

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

  test('Should set body[data-component-id] via setComponent()', () => {
    context.setComponentId('component-one');
    expect(context.getAttribute(document.body, 'data-component-id')).toEqual(
      'component-one',
    );
  });

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

});
