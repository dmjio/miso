function Y(T,E){var V=function(){var N=document.getElementById(T);if(N&&N.focus)N.focus()};E>0?setTimeout(V,E):V()}function Z(T,E){var V=function(){var N=document.getElementById(T);if(N&&N.blur)N.blur()};E>0?setTimeout(V,E):V()}function _(T){document.body.setAttribute("data-component-id",T)}function $(T,E,V,N,M,J){var B={method:E,headers:N};if(V)B.body=V;fetch(T,B).then((H)=>{if(!H.ok)throw new Error(H.statusText);return H.json()}).then(M).catch(J)}function A(T){if(T.children.length===0)return!1;var E=!0;for(let V of T.children)if(!V.key){E=!1;break}return E}var W="1.9.0.0";function D(T){var E=c(l(),T);return E.shouldSync=A(E),E}function c(T,E){return Object.assign({},T,E)}function l(){return{props:{},css:{},children:[],ns:"html",domRef:null,tag:"div",key:null,events:{},onDestroyed:()=>{},onBeforeDestroyed:()=>{},onCreated:()=>{},onBeforeCreated:()=>{},shouldSync:!1,type:"vnode"}}function S(T,E,V){if(!T&&!E)return;else if(!T&&E)TT(E,V);else if(!E)a(T,V);else if(T.type===E.type)v(T,E,V);else I(T,E,V)}function I(T,E,V){if(O(T),E.type==="vtext")E.domRef=document.createTextNode(E.text),V.replaceChild(E.domRef,T.domRef);else g(E,(N)=>{V.replaceChild(N,T.domRef)});f(T)}function a(T,E){O(T),E.removeChild(T.domRef),f(T)}function v(T,E,V){if(T.type==="vtext"){if(T.text!==E.text)T.domRef.textContent=E.text;E.domRef=T.domRef;return}if(T.tag===E.tag&&E.key===T.key&&E["data-component-id"]===T["data-component-id"])E.domRef=T.domRef,K(T,E);else I(T,E,V)}function f(T){j(T);for(let E in T.children)f(T.children[E])}function j(T){if(T.onDestroyed)T.onDestroyed();if(T.type==="vcomp")n(T)}function b(T){if(T.onBeforeDestroyed)T.onBeforeDestroyed()}function O(T){if(T.type==="vcomp"&&T.onBeforeUnmounted)T.onBeforeUnmounted();b(T);for(let E in T.children)O(T.children[E])}function h(T){if(T.onCreated)T.onCreated();if(T.type==="vcomp")e(T)}function d(T){if(T.onBeforeCreated)T.onBeforeCreated()}function K(T,E){if(E.type!=="vtext"){if(!T)T=D({});if(s(T.props,E.props,E.domRef,E.ns==="svg"),r(T.css,E.css,E.domRef),E.type==="vnode")o(T,E,E.domRef)}}function s(T,E,V,N){var M;for(let J in T)if(M=E[J],M===void 0)if(N||!(J in V))V.removeAttribute(T[J]);else V[J]="";else{if(M===T[J]&&J!=="checked"&&J!=="value")continue;if(N)if(J==="href")V.setAttributeNS("http://www.w3.org/1999/xlink","href",M);else V.setAttribute(J,M);else if(J in V&&!(J==="list"||J==="form"))V[J]=M;else V.setAttribute(J,M)}for(let J in E){if(T&&T[J])continue;if(M=E[J],N)if(J==="href")V.setAttributeNS("http://www.w3.org/1999/xlink","href",M);else V.setAttribute(J,M);else if(J in V&&!(J==="list"||J==="form"))V[J]=E[J];else V.setAttribute(J,M)}}function r(T,E,V){var N;for(let M in T)if(N=E[M],!N)V.style[M]="";else if(N!==T[M])V.style[M]=N;for(let M in E){if(T&&T[M])continue;V.style[M]=E[M]}}function o(T,E,V){if(T.shouldSync&&E.shouldSync)ET(T.children,E.children,V);else{let N=E.children.length>T.children.length?E.children.length:T.children.length;for(let M=0;M<N;M++)S(T.children[M],E.children[M],V)}}function t(T){if(T.ns==="svg")T.domRef=document.createElementNS("http://www.w3.org/2000/svg",T.tag);else if(T.ns==="mathml")T.domRef=document.createElementNS("http://www.w3.org/1998/Math/MathML",T.tag);else T.domRef=document.createElement(T.tag)}function g(T,E){d(T),t(T),E(T.domRef),K(null,T),h(T)}function n(T){if("onUnmounted"in T)T.onUnmounted();T.unmount()}function e(T){let E=T["data-component-id"];if(document.querySelectorAll("[data-component-id='"+E+"']").length>0){console.error('AlreadyMountedException: Component "'+E+"' is already mounted");return}if(T.domRef.setAttribute("data-component-id",E),T.onBeforeMounted)T.onBeforeMounted();T.mount((N)=>{if(T.children.push(N),T.domRef.appendChild(N.domRef),T.onMounted)T.onMounted()})}function TT(T,E){if(T.type==="vtext")T.domRef=document.createTextNode(T.text),E.appendChild(T.domRef);else g(T,(V)=>{E.appendChild(V)})}function ET(T,E,V){var N=0,M=0,J=T.length-1,B=E.length-1,H,R,z,G,U,X,w;for(;;){if(M>B&&N>J)break;if(R=E[M],z=E[B],U=T[N],G=T[J],N>J)S(null,R,V),V.insertBefore(R.domRef,U?U.domRef:null),T.splice(M,0,R),M++;else if(M>B){H=J;while(J>=N)V.removeChild(T[J--].domRef);T.splice(N,H-N+1);break}else if(U.key===R.key)S(T[N++],E[M++],V);else if(G.key===z.key)S(T[J--],E[B--],V);else if(U.key===z.key&&R.key===G.key)VT(G.domRef,U.domRef,V),NT(T,N,J),S(T[N++],E[M++],V),S(T[J--],E[B--],V);else if(U.key===z.key)V.insertBefore(U.domRef,G.domRef.nextSibling),T.splice(J,0,T.splice(N,1)[0]),S(T[J--],E[B--],V);else if(G.key===R.key)V.insertBefore(G.domRef,U.domRef),T.splice(N,0,T.splice(J,1)[0]),S(T[N++],R,V),M++;else{X=!1,H=N;while(H<=J){if(T[H].key===R.key){X=!0,w=T[H];break}H++}if(X)T.splice(N,0,T.splice(H,1)[0]),S(T[N++],R,V),V.insertBefore(w.domRef,T[N].domRef),M++;else g(R,(i)=>{V.insertBefore(i,U.domRef)}),T.splice(N++,0,R),M++,J++}}}function VT(T,E,V){let N=T.nextSibling;V.insertBefore(T,E),V.insertBefore(E,N)}function NT(T,E,V){let N=T[E];T[E]=T[V],T[V]=N}function C(T,E,V,N){for(let M of E)T.addEventListener(M.name,function(J){y(J,T,V,N)},M.capture)}function P(T,E,V,N){for(let M of E)T.removeEventListener(M.name,function(J){y(J,T,V,N)},M.capture)}function y(T,E,V,N){V(function(M){if(T.target)k(T,M,MT(E,T.target),[],N)})}function MT(T,E){var V=[];while(T!==E)V.unshift(E),E=E.parentNode;return V}function k(T,E,V,N,M){if(!V.length){if(M)console.warn('Event "'+T.type+'" did not find an event handler to dispatch on',E,T);return}else if(V.length>1){N.unshift(E);for(let J of E.children){if(J.type==="vcomp")continue;if(J.domRef===V[1]){k(T,J,V.slice(1),N,M);break}}}else{let J=E.events[T.type];if(J){let B=J.options;if(B.preventDefault)T.preventDefault();if(J.runEvent(T),!B.stopPropagation)x(N,T)}else x(N,T)}}function x(T,E){for(let V of T)if(V.events[E.type]){let N=V.events[E.type],M=N.options;if(M.preventDefault)E.preventDefault();if(N.runEvent(E),M.stopPropagation){E.stopPropagation();break}}}function q(T,E){if(typeof T[0]==="object"){var V=[];for(var N=0;N<T.length;N++)V.push(q(T[N],E));return V}for(let H of T)E=E[H];var M;if(E instanceof Array||"length"in E&&E.localName!=="select"){M=[];for(var J=0;J<E.length;J++)M.push(q([],E[J]));return M}M={};for(var B in JT(E)){if(E.localName==="input"&&(B==="selectionDirection"||B==="selectionStart"||B==="selectionEnd"))continue;if(typeof E[B]=="string"||typeof E[B]=="number"||typeof E[B]=="boolean")M[B]=E[B]}return M}function JT(T){var E={},V=0;do{var N=Object.getOwnPropertyNames(T);for(V=0;V<N.length;V++)E[N[V]]=null}while(T=Object.getPrototypeOf(T));return E}function BT(T){var E=0,V=T.length>0?[T[0]]:[];for(var N=1;N<T.length;N++){if(V[E].type==="vtext"&&T[N].type==="vtext"){V[E].text+=T[N].text;continue}V[++E]=T[N]}return V}function HT(T){var E=0,V;if(!T)if(document.body.childNodes.length>0)V=document.body.firstChild;else V=document.body.appendChild(document.createElement("div"));else if(T.childNodes.length===0)V=T.appendChild(document.createElement("div"));else{while(T.childNodes[E]&&(T.childNodes[E].nodeType===3||T.childNodes[E].localName==="script"))E++;if(!T.childNodes[E])V=document.body.appendChild(document.createElement("div"));else V=T.childNodes[E]}return V}function m(T,E,V){let N=HT(E);if(!F(T,V,N)){if(T)console.warn("Could not copy DOM into virtual DOM, falling back to diff");while(N.firstChild)N.removeChild(N.lastChild);return V.domRef=N,K(null,V),!1}else if(T)if(!Q(V))console.warn("Integrity check completed with errors");else console.info("Successfully prerendered page");return!0}function L(T,E,V){if(T)console.warn("VTree differed from node",E,V)}function u(T){if(T.substr(0,1)=="#"){let E=(T.length-1)/3,V=[17,1,0.062272][E-1];return[Math.round(parseInt(T.substr(1,E),16)*V),Math.round(parseInt(T.substr(1+E,E),16)*V),Math.round(parseInt(T.substr(1+2*E,E),16)*V)]}else return T.split("(")[1].split(")")[0].split(",").map((E)=>{return+E})}function Q(T){return p(!0,T)}function p(T,E){if(E.type=="vtext"){if(E.domRef.nodeType!==3)console.warn("VText domRef not a TEXT_NODE",E),T=!1;else if(E.text!==E.domRef.textContent)console.warn("VText node content differs",E),T=!1}else{if(E.tag.toUpperCase()!==E.domRef.tagName)console.warn("Integrity check failed, tags differ",E.tag.toUpperCase(),E.domRef.tagName),T=!1;if("children"in E&&E.children.length!==E.domRef.childNodes.length)console.warn("Integrity check failed, children lengths differ",E,E.children,E.domRef.childNodes),T=!1;for(let V in E.props)if(V==="href"){let N=window.location.origin+"/"+E.props[V],M=E.domRef[V],J=E.props[V];if(N!==M&&J!==M&&J+"/"!==M&&N+"/"!==M)console.warn("Property "+V+" differs",E.props[V],E.domRef[V]),T=!1}else if(V==="height"||V==="width"){if(parseFloat(E.props[V])!==parseFloat(E.domRef[V]))console.warn("Property "+V+" differs",E.props[V],E.domRef[V]),T=!1}else if(V==="class"||V==="className"){if(E.props[V]!==E.domRef.className)console.warn("Property class differs",E.props[V],E.domRef.className),T=!1}else if(!E.domRef[V]){if(E.props[V]!==E.domRef.getAttribute(V))console.warn("Property "+V+" differs",E.props[V],E.domRef.getAttribute(V)),T=!1}else if(E.props[V]!==E.domRef[V])console.warn("Property "+V+" differs",E.props[V],E.domRef[V]),T=!1;for(let V in E.css)if(V==="color"){if(u(E.domRef.style[V]).toString()!==u(E.css[V]).toString())console.warn("Style "+V+" differs",E.css[V],E.domRef.style[V]),T=!1}else if(E.css[V]!==E.domRef.style[V])console.warn("Style "+V+" differs",E.css[V],E.domRef.style[V]),T=!1;for(let V of E.children){let N=p(T,V);T=T&&N}}return T}function F(T,E,V){switch(E.type){case"vtext":E.domRef=V;break;default:E.domRef=V,E.children=BT(E.children),h(E);for(var N=0;N<E.children.length;N++){let M=E.children[N],J=V.childNodes[N];if(!J)return L(T,M,J),!1;switch(M.type){case"vtext":if(J.nodeType!==3)return L(T,M,J),!1;if(M.text===J.textContent)M.domRef=V.childNodes[N];else return L(T,M,J),!1;break;case"vcomp":M.mount((B)=>{M.children.push(B),F(T,M,V.childNodes[N])});break;default:if(J.nodeType!==1)return!1;M.domRef=V.childNodes[N],F(T,M,M.domRef)}}}return!0}globalThis.miso={};globalThis.miso.diff=S;globalThis.miso.hydrate=m;globalThis.miso.version=W;globalThis.miso.delegate=C;globalThis.miso.callBlur=Z;globalThis.miso.callFocus=Y;globalThis.miso.eventJSON=q;globalThis.miso.fetchJSON=$;globalThis.miso.undelegate=P;globalThis.miso.integrityCheck=Q;globalThis.miso.setBodyComponent=_;
