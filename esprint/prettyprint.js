var p=require("esprima")
var g=require("escodegen")

var file = process.argv[2]

//load
fs = require('fs')
fs.readFile(file, 'utf8', function (err,data) {
  if (err) {
    console.error(err);
    process.exit(1);
  }
  var ast = p.parse(data, {loc:true});
  fs.writeFile(file+".ast", JSON.stringify(ast),function(x){if(x) {console.error(err);process.exit(1);}})
  fs.writeFile(file+".pp", g.generate(ast),function(x){if(x) {console.error(err);process.exit(1);}});
});
