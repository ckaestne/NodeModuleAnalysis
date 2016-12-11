package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.datalog.{Datalog, _}
import edu.cmu.cs.nodesec.parser.FunctionBody

/**
  * Analyse policies on one or multiple methods (using datalog)
  *
  * Method composition works along hierarchies (important for handling closures).
  * A method is always composed with all its children. (To compose two independent
  * methods, create an outer method.)
  *
  * Analysis is performed by first computing method summaries for all
  * methods and then translating them into datalog rules.
  *
  *
  * Created by ckaestne on 11/25/16.
  */
object MethodCompositionAnalysis {






  import AnalysisHelper._
  import VariableHelper._


  def analyzeScript(p: FunctionBody, policy: Policy, withGlobals: Boolean = false): Seq[PolicyViolation] = {
    val fun = if (withGlobals) cfgWithGlobals(p) else cfgScript(p)
    analyze(fun, policy, Some(fun))
  }

  def getAllInnerFun(fun: Fun): Set[Fun] =
    fun.innerFunctions.map(getAllInnerFun).flatten + fun

  def analyze(fun: Fun, policy: Policy, mainFun: Option[Fun] = None): Seq[PolicyViolation] = {
    var functions = getAllInnerFun(fun)


    val summaries = for (fun <- functions)
      yield (fun, new IntraMethodAnalysis().analyze(fun))

    val result = compose(summaries)


    policy(result, mainFun.getOrElse(fun), summaries)
  }


  /**
    * this method composes the method summaries produced by `IntraMethodAnalysis`
    *
    * context insensitive for now (context sensitivity through copying requires cycle detection
    * for calls among modules first)
    */
  def compose(methodSummaries: Set[(Fun, MethodSummary)]): Datalog = {

    //    var inclusionEdges: Set[(Obj, Obj)] = Set()

    val datalog = new Datalog()

    val rules =
      """
        |% transitive pointsTo relation
        |pt(FROM, TO) :- pt(FROM, B), pt(B, TO).
        |
        |% resolved call graph edges
        |% from origin OFUNID (with object representing the target TARGETOBJ and the resulting value RETVAL) to the target function TFUNID
        |call(OFUNID,TARGETOBJ,RETVAL,TFUNID) :- invoke(OFUNID,TARGETOBJ,RETVAL), functiondecl(U, TARGETOBJ, TFUNID).
        |call(OFUNID,TARGETOBJ,RETVAL,TFUNID) :- invoke(OFUNID,TARGETOBJ,RETVAL), pt(TARGETOBJ, O), functiondecl(U, O, TFUNID).
        |
        |% link returned value of function call to return-value of the target function
        |pt(R, O) :- call(U1, U2, R, F), return(F, O).
        |% link formal parameter to actual parameter
        |pt(A, B) :- actual(T, Z, B), call(U1, T, X, F), formal(F, Z, A).
        |
        |% merge members pointing to the same obj:
        |pt(A, B) :- member(X, F, A), member(X, F, B).
        |pt(A, B) :- member(X, F, A), pt(X, Y), member(Y, F, B).
        |
        |% link scope to scope of closure
        |parentscope(OUTER, INNER) :- functiondecl(OUTER, U, INNER).
        |pt(A, B):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "closure", B).
        |pt(A, B):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "local", B).
        |
        |%    TODO: the following would allow writes to be propagated back, but makes everything absolutely
        |%    conservative by merging local and global scopes of all function that contain any other function decl.
        |%    for now we rather have a policy against writing to outer environments
        |% scope needs to be shared both directions, as inner functions can update values in outer scopes
        |pt(B, A):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "closure", B).
        |pt(B, A):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "local", B).
        |      """.stripMargin
    datalog.loadRules(rules)





    println(datalog.ruleStr + "%%%")

    for ((function, summary) <- methodSummaries) {
      val facts = summaryToDatalog(function, summary)
      facts.foreach(datalog.load)
      facts.map(println)
      println("%")
    }


    datalog
  }

  def summaryToDatalog(fun: Fun, env: MethodSummary): List[DRelation] = {
    var result: List[DRelation] = Nil

    for ((arg, idx) <- fun.args.zipWithIndex;
         obj <- env.lookup(arg)._1)
      result ::= DFormal(fun, idx, obj)

    for (retvar <- Set(returnVariable); obj <- env.lookup(retvar)._1)
      result ::= DReturn(fun, obj)

    for ((obj, children) <- env.members;
         (field, fieldvalues) <- children;
         fieldvalue <- fieldvalues)
      result ::= DMember(fun, obj, field, fieldvalue)

    for ((call, retVals) <- env.calls;
         retVal <- retVals;
         target <- retVal.target) {
      result ::= DInvoke(fun, target, retVal)
      for ((argSet, idx) <- retVal.args.zipWithIndex; arg <- argSet)
        result ::= DActual(fun, target, idx, arg)
    }

    for ((obj, targetFun) <- env.functionPtrs)
      result ::= DFunctionDecl(fun, obj, targetFun)

    if (fun.uniqueId != "global#") {
      for ((o, f, v) <- env.writes)
        result ::= DStore(fun, o, f, v)

      for ((o, f, v) <- env.reads)
        result ::= DLoad(fun, o, f, v)
    }

    result ::= DScope(fun, "local", env.localScopeObj)
    result ::= DScope(fun, "closure", env.closureObj)


    result
  }


}
