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

  def cfgWithGlobals(script: FunctionBody): Fun =
    CFGBuilder.toFun(wrapScript(wrapWithGlobals(script)))

  def cfgScript(script: FunctionBody): Fun =
    CFGBuilder.toFun(wrapScript(script))

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