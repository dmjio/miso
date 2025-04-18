globalThis.renderPage = function() {
  var page = __CreatePage("0", 0);
  var pageId = __GetElementUniqueID(page);
  var el = __CreateView(pageId);
  __AppendElement(page, el);
  var el1 = __CreateText(pageId);
  __AppendElement(el, el1);
  var el2 = __CreateRawText("Hello Lynx x Webpack");
  __AppendElement(el1, el2);
}

globalThis.processData = function () {

}
