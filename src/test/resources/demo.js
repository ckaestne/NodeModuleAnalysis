var net = require("http");
var all = 3;

module.exports.foo = function(foo) {
  return foo + all;
};

module.exports.leak = function(x) {
    return net.send(x, all);
};

module.exports.more = function(x1) {
    module.exports.evenmore = function(y) { return y - 1; };
};

var page = net.load("page");
net.callback(function(x){
    x.send(page);
});