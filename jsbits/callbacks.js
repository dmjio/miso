window['syncCallback_'] = function (obj) {
  return h$makeCallback(h$runSyncReturn, [false], obj);
}

window['syncCallback1_'] = function (obj) {
  return h$makeCallbackApply(1, h$runSyncReturn, [false], $1);
}

window['asyncCallback'] = function (obj) {
  return h$makeCallback(h$run, [], obj);
}

window['asyncCallback1'] = function (obj) {
  return h$makeCallbackApply(1, h$run, [], obj);
}

window['asyncCallback2'] = function (obj) {
  return h$makeCallbackApply(2, h$run, [], obj);
}
