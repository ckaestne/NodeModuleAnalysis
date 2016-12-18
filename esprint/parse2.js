var p=require("esprima")

var fin = process.argv[2]
var fout = process.argv[3]

//load
fs = require('fs')
fs.readFile(fin, 'utf8', function (err,data) {
  if (err) {
    console.error(err);
    process.exit(1);
  }
  var ast = p.parse(data, {loc:true});
  fs.writeFile(fout, JSON.stringify(ast),function(x){if(x) {console.error(err);process.exit(1);}})
});
