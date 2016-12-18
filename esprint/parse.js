var p=require("esprima")


process.stdin.setEncoding('utf8');
var input=""
process.stdin.on('readable', function () {
  input = input + process.stdin.read()
});

process.stdin.on('end', function () {
  var ast = p.parse(input, {loc:true});
  console.log(JSON.stringify(ast));
  console.log("\n")
  console.log("done.\n")
});
process.stdin.on('close', function () {
  console.log("\nclose\n")
process.exit()
})
