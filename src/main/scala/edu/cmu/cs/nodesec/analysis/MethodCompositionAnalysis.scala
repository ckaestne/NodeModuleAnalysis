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

    println("callToRequire(A):-call(X,A), pt(A, " + Integer.toHexString(mainFun.hashCode()) + "#param-require).\n" +
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
        |pt(A,B) :- call(A, F), return(F, B).
        |pt(A,B) :- arg(F,Z,A), callarg(F,Z,B).
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
      result ::= s"arg($prefix, ${idx + 1}, $prefix$obj)."

    for (retvar <- Set(returnVariable); obj <- env.lookup(retvar)._1)
      result ::= s"return(${prefix}, $prefix$obj)."

    for ((obj, children) <- env.members;
         (field, fieldvalues) <- children;
         fieldvalue <- fieldvalues)
      result ::= s"member(${prefix}$obj, $field, $prefix$fieldvalue)."

    for ((call, retVals) <- env.calls;
         retVal <- retVals;
         target <- retVal.target) {
      val targetStr = target match {
        case f: Fun => Integer.toHexString(f.f.hashCode()) + "#"
        case _ => prefix + target //TODO fixme
      }
      result ::= s"call($prefix$retVal, $targetStr)."
      for ((argSet, idx) <- retVal.args.zipWithIndex; arg <- argSet)
        result ::= s"callarg($targetStr, ${idx + 1}, $prefix$arg)."
    }




    result.reverse.mkString("\n")
  }


}
