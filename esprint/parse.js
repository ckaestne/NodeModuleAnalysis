var p=require("esprima")
var g=require("escodegen")


process.stdin.setEncoding('utf8');
var input=""
process.stdin.on('readable', function () {
  input = input + process.stdin.read()
});

process.stdin.on('end', function () {
  var ast = p.parse(input, {loc:true});
  console.log(JSON.stringify(ast));
  console.log("\n")
});
process.stdin.on('close', function () {
  console.log("\nclose\n")
process.exit()
})
