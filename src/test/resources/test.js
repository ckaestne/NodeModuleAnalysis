//var x = require;
//x("foo");

function T(){ this.foo = function(){ return 0; }; }; //dT , pT
var t = new T(); //a1
T.prototype.bar = function(){ return 1; }; //dbar, pbar
t.bar(); // return 1