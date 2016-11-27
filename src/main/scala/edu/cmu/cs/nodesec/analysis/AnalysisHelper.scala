package edu.cmu.cs.nodesec.analysis

import java.io.FileReader

import edu.cmu.cs.nodesec.parser.{FunExpr, FunctionBody, Id, JSParser}

/**
  * Created by ckaestne on 11/26/16.
  */
object AnalysisHelper {

  val globalJsFile = "src/main/resources/global.js"

  lazy val globalsVM = {
    val p = new JSParser()
    val parsed = p.parseAll(p.Program, new FileReader(globalJsFile))
    assert(parsed.successful, "parsing globals failed: " + parsed)
    FunExpr(None, Nil, parsed.get).toVM()
  }

  //  lazy val globalsSummary = new IntraMethodAnalysis().analyze(globalsVM)

  def wrapScript(p: FunctionBody): FunDecl =
    FunExpr(None, List(Id("module"), Id("require"), Id("exports")), p).toVM()

  def wrapWithGlobals(f: FunDecl): FunDecl = {
    //returning a version of the global file to which the provided function declaration
    //is added and then called
    val vm = globalsVM
    val newBody =
      Call(new AnonymousVariable(), f.v, VariableHelper.thisVar,
        List(LocalVariable("module"), LocalVariable("require"), LocalVariable("exports"))) ++
        f ++ vm.body

    new FunDecl(vm.v, vm.args, vm.localVariables, newBody) {
      override lazy val uniqueId = "global#"
    }
  }

  val returnVariable = LocalVariable("$return")
}
