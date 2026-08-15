declare module 'text-encoding' {
  export class TextEncoder {
    encode(input?: string): Uint8Array;
  }
  export class TextDecoder {
    decode(input?: ArrayBufferView): string;
  }
}

declare module 'jsbi' {
  const JSBI: any;
  export default JSBI;
}
