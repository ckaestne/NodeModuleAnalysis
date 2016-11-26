package edu.cmu.cs.nodesec.analysis

/**
  * Created by ckaestne on 11/25/16.
  */
class MethodCompositionAnalysis {

  import AnalysisHelper._

  def collectFunDecls(s: Statement): Set[FunDecl] = s match {
    case f: FunDecl => collectFunDecls(f.body) + f
    case Sequence(inner) => inner.foldLeft(Set[FunDecl]())(_ ++ collectFunDecls(_))
    case LoopStatement(inner) => collectFunDecls(inner)
    case ConditionalStatement(a, b) => collectFunDecls(a) ++ collectFunDecls(b)
    case _ => Set()
  }


  def analyzeScript(p: Statement): Env = {
    val mainFun = AnalysisHelper.wrapScript(p)
    val funDecls = collectFunDecls(mainFun)

    val summaries = for (funDecl <- funDecls)
      yield (funDecl, new IntraMethodAnalysis().analyze(funDecl))

    val result = compose(summaries)

    println("callToRequire(A):-invoke(X,A), pt(A, " + Integer.toHexString(mainFun.hashCode()) + "#param-require).\n" +
      "callToRequire(A)?")

    result
  }


  /**
    * this method composes the method summaries produced by `IntraMethodAnalysis`
    *
    * context insensitive for now (context sensitivity through copying requires cycle detection
    * for calls among modules first)
    */
  def compose(methodSummaries: Set[(FunDecl, Env)]): Env = {
    var result = Env.empty

    //    var inclusionEdges: Set[(Obj, Obj)] = Set()

    println(
      """
        |%rules
        |pt(A,C) :- pt(A,B), pt(B,C).
        |pt(R,O) :- invoke(T, R), functionptr(T, F), return(F, O).
        |pt(R,O) :- invoke(T, R), pt(T, Q), functionptr(Q, F), return(F, O).
        |pt(A,B) :- actual(T,Z,B), functionptr(T, F), formal(F,Z,A).
        |pt(A,B) :- actual(T,Z,B), pt(T, Q), functionptr(Q, F), formal(F,Z,A).
        |pt(A,B) :- member(X, F, A), member(X, F, B).
        |pt(A,B) :- member(X, F, A), member(Y, F, B), pt(X, Y).
        |%""".stripMargin)

    methodSummaries.map(a => summaryToDatalog(a._1, a._2) + "\n%").foreach(println)



    result
  }

  def summaryToDatalog(fun: FunDecl, env: Env): String = {
    val prefix = Integer.toHexString(fun.hashCode()) + "#"
    var result: List[String] = Nil

    result ::= s"fun($prefix)."
    for ((arg, idx) <- fun.args.zipWithIndex;
         obj <- env.lookup(arg)._1)
      result ::= s"formal($prefix, ${idx + 1}, $prefix$obj)."

    for (retvar <- Set(returnVariable); obj <- env.lookup(retvar)._1)
      result ::= s"return(${prefix}, $prefix$obj)."

    for ((obj, children) <- env.members;
         (field, fieldvalues) <- children;
         fieldvalue <- fieldvalues)
      result ::= s"member(${prefix}$obj, $field, $prefix$fieldvalue)."

    for ((call, retVals) <- env.calls;
         retVal <- retVals;
         target <- retVal.target) {
      result ::= s"invoke($prefix$target, $prefix$retVal)."
      for ((argSet, idx) <- retVal.args.zipWithIndex; arg <- argSet)
        result ::= s"actual($prefix$target, ${idx + 1}, $prefix$arg)."
    }

    for ((obj, fun) <- env.functionPtrs)
      result ::= s"functionptr($prefix$obj, ${Integer.toHexString(fun.hashCode())}#)."



    result.reverse.mkString("\n")
  }


}
