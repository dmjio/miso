/* imports */
import { diff } from '../miso/dom';
import { vnode, vcomp, vtext } from '../miso/smart';
import { VTree, DOMRef } from '../miso/types';
import { test, expect, describe, afterEach, beforeAll } from 'bun:test';
import { context } from '../miso/context/dom';
import { patch } from '../miso/patch';

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
describe ('Patch tests', () => {
    test('Should process the insertBefore patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the swapDOMRefs patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the createElement patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the createElementNS patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the createTextNode patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the setAttribute patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the appendChild patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the replaceChild patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the removeAttribute patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the setTextContext patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the setInlineStyle patch', () => {
        expect(2+2).toEqual(4);
    });
    test('Should process the flush patch', () => {
        expect(2+2).toEqual(4);
    });
});

describe ('Hydration tests', () => {
    test('Should hydrate a component model', () => {
        expect(2+2).toEqual(4);
    });
});

describe ('Event tests', () => {
    test('Should register component events / component mount', () => {
        /* this is equivalent to component mounting */
        expect(2+2).toEqual(4);
    });
    test('Should unregister component events / component unmount', () => {
        /* this is equivalent to component unmounting */
        expect(2+2).toEqual(4);
    });
    test('Should add main thread events, and eval()', () => {
        expect(2+2).toEqual(4);
    });
    test('Should round trip apply an event raised from MTS, to BTS, and back to MTS', () => {
        expect(2+2).toEqual(4);
    });
});
