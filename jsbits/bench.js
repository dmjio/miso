var benchmark = require('vdom-benchmark-base');

var NAME = 'miso';
var VERSION = '0.0.1';

function BenchmarkImpl(container, a, b) {
  this.container = container;
  this.a = a;
  this.b = b;
  this.miso = {};
}

BenchmarkImpl.prototype.setUp = function() {
  console.log('setting up...');
};

BenchmarkImpl.prototype.tearDown = function() {
    console.log('tearing down...');
    updateNode(this.miso, null);
    this.container.removeChild(this.miso.element);
};

BenchmarkImpl.prototype.render = function() {
    console.log('render', 'this.a ->', this.a, 'miso ->', this.miso);
    renderNode(this.miso, this.a);
    console.log(this.miso, 'misoso');
    this.container.appendChild(this.miso.element);
};

BenchmarkImpl.prototype.update = function() {
    console.log('update', 'this.b ->', this.b, 'miso ->', this.miso);
    updateNode(this.miso, this.b);
};

document.addEventListener('DOMContentLoaded', function(e) {
  console.log('loaded', e);
  benchmark(NAME, VERSION, BenchmarkImpl);
}, false);
