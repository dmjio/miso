import { callFocus, callBlur, setComponent } from '../miso/util';
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

  test('Should set body[data-component-id] via setComponent()', () => {
    setComponent(document.body, 'component-one');
    expect(document.body.getAttribute('data-component-id')).toEqual(
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
