declare var __BACKGROUND__: boolean | undefined;
declare var __MAIN_THREAD__: boolean | undefined;

declare var lynx: {
  requestAnimationFrame: (cb: (time: number) => void) => number;
  cancelAnimationFrame: (id: number) => void;
  [key: string]: any;
};
