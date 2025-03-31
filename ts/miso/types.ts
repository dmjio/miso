/* core type for virtual DOM */
type VTree = {
   type : 'vtext' | 'vnode' | 'vcomp',
   ns : 'html' | 'svg' | 'mathml',
   domRef : any,
   text : string,
   tag: string,
   key: string,
   props: Map<string,string>,
   css: Map<string,string>,
   events: Map<string,EventObject>,
   'data-component-id' :string,
   children : Array<VTree>,
   onDestroyed : () => void,
   onCreated : () => void,
   onBeforeDestroyed : () => void,
   mount: (VTree) => void,
   unmount: (any) => void
};

type EventObject = {
 options : Options,
 runEvent : (Event) => void,
};

type Options = {
 preventDefault : boolean,
 stopPropagation : boolean
};

type EventCapture = {
 name : string,
 capture : boolean
};

export {
  VTree, EventCapture, EventObject, Options
};
