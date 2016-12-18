package edu.cmu.cs.nodesec.analysis

import java.io.FileReader

import edu.cmu.cs.nodesec.parser._

/**
  * Created by ckaestne on 11/26/16.
  */
object AnalysisHelper {

  val globalJsFile = "src/main/resources/global.js"

  def getCFG(fun: FunExpr): Fun = CFGBuilder.toFun(fun)

  lazy val globalsAST: FunctionBody = {
    val p = new JSParser()
    val parsed = p.parseAll(p.Program, new FileReader(globalJsFile))
    assert(parsed.successful, "parsing globals failed: " + parsed)
    parsed.get
  }

  //  lazy val globalsSummary = new IntraMethodAnalysis().analyze(globalsVM)

  def wrapScript(p: FunctionBody): Function =
    FunExpr(None, List(Id("module"), Id("require"), Id("exports")), p)

  def wrapWithGlobals(f: FunctionBody): FunctionBody = {
    //returning a version of the global file to which the provided function declaration
    //is added and then called
    val globals = globalsAST
    val call = ExpressionStmt(FunCall(
      FunExpr(None, List(Id("module"), Id("require"), Id("exports")), f),
      List(Id("module"), Id("require"), Id("exports"))
    ))
    FunctionBody(call :: globals.inner)
  }

  /**
    * turn a script/module into a CFG
    *
    * this injects the module, require, and export arguments, but
    * also provides global objects such as `console` from
    * `global.js`
    */
  def cfgWithGlobals(scriptAst: FunctionBody): Fun =
    CFGBuilder.toFun(wrapScript(wrapWithGlobals(scriptAst)))

  /**
    * turn a script/module (i.e. some code that is wrapped
    * by the function providing the module, require, exports arguments)
    * into a CFG
    */
  def cfgScript(scriptAst: FunctionBody): Fun =
    CFGBuilder.toFun(wrapScript(scriptAst))

  //  def summarizeFunction(fun: Fun): MethodSummary =
  //    new IntraMethodAnalysis().analyze(fun)

}

object NameHelper {
  var objectCounter = 0

  def genObjectName = {
    objectCounter += 1
    "obj" + objectCounter
  }

  var functionCounter = 0

  def genFunctionName = {
    functionCounter += 1
    "fun" + functionCounter
  }
}