//package edu.cmu.cs.nodesec.analysis
//
//import edu.cmu.cs.nodesec.datalog.{Datalog, _}
//import edu.cmu.cs.nodesec.parser.FunctionBody
//
///**
//  * Analyse policies on one or multiple methods (using datalog)
//  *
//  * Method composition works along hierarchies (important for handling closures).
//  * A method is always composed with all its children. (To compose two independent
//  * methods, create an outer method.)
//  *
//  * Analysis is performed by first computing method summaries for all
//  * methods and then translating them into datalog rules.
//  *
//  *
//  * Created by ckaestne on 11/25/16.
//  */
//object MethodCompositionAnalysis {
//
//
//  import AnalysisHelper._
//  import VariableHelper._
//
//
//  def analyzeScript(p: FunctionBody, policy: Policy, withGlobals: Boolean = false): Seq[PolicyViolation] = {
//    val fun = if (withGlobals) cfgWithGlobals(p) else cfgScript(p)
//    analyze(fun, policy, Some(fun))
//  }
//
//  def getAllInnerFun(fun: Fun): Set[Fun] =
//    fun.innerFunctions.map(getAllInnerFun).flatten + fun
//
//
//  /**
//    * hierarchical composition,
//    *
//    * returns datalog facts and updated set of closure variables
//    */
//  def composeWithInnerFunctions(fun: Fun): (Set[ExternalVariable], Seq[DFact]) = {
//    //start composition from child to parent
//    val innerFunctions = fun.innerFunctions.map(f => (f, composeWithInnerFunctions(f)))
//    val innerFacts = innerFunctions.toSeq.flatMap(_._2._2)
//
//    val summary = new IntraMethodAnalysis().analyze(fun)
//    var facts = summaryToDatalog(fun, summary)
//
//    //connect closure variables
//    val innerClosureVariables = innerFunctions.flatMap(_._2._1)
//    val closureToLocal = innerClosureVariables.map(cl => cl -> fun.localOrArgs.find(_.name == cl.name)).filter(_._2.nonEmpty).toMap
//    val propagatedClosure = innerClosureVariables -- closureToLocal.keys
//    val outerClosure = fun.closureVariables ++ propagatedClosure
//    for ((innerFun, (closureVars, _)) <- innerFunctions; closureVar <- closureVars) {
//      if (closureToLocal contains closureVar)
//        facts :+= DClosureToLocal(fun, closureToLocal(closureVar).get, innerFun, closureVar)
//      if (propagatedClosure contains closureVar)
//        facts :+= DClosureToClosure(fun, innerFun, closureVar)
//    }
//
//    (outerClosure, innerFacts ++ facts)
//  }
//
//
//  def analyze(fun: Fun, policy: Policy, mainFun: Option[Fun] = None): Seq[PolicyViolation] = {
//    var functions = getAllInnerFun(fun)
//
//
//    val summaries = for (fun <- functions)
//      yield (fun, new IntraMethodAnalysis().analyze(fun))
//
//    val result = compose(summaries)
//
//
//    policy(result, mainFun.getOrElse(fun), summaries)
//  }
//
//
//  /**
//    * this method composes the method summaries produced by `IntraMethodAnalysis`
//    *
//    * context insensitive for now (context sensitivity through copying requires cycle detection
//    * for calls among modules first)
//    */
//  def compose(methodSummaries: Set[(Fun, MethodSummary)]): Datalog = {
//
//    //    var inclusionEdges: Set[(Obj, Obj)] = Set()
//
//    val datalog = new Datalog()
//
//    val rules =
//      """
//        |% transitive pointsTo relation
//        |pt(FROM, TO) :- pt(FROM, B), pt(B, TO).
//        |
//        |% resolved call graph edges
//        |% from origin OFUNID (with object representing the target TARGETOBJ and the resulting value RETVAL) to the target function TFUNID
//        |call(OFUNID,TARGETOBJ,RETVAL,TFUNID) :- invoke(OFUNID,TARGETOBJ,RETVAL), functiondecl(U, TARGETOBJ, TFUNID).
//        |call(OFUNID,TARGETOBJ,RETVAL,TFUNID) :- invoke(OFUNID,TARGETOBJ,RETVAL), pt(TARGETOBJ, O), functiondecl(U, O, TFUNID).
//        |
//        |% link returned value of function call to return-value of the target function
//        |pt(R, O) :- call(U1, U2, R, F), return(F, O).
//        |% link formal parameter to actual parameter
//        |pt(A, B) :- actual(T, Z, B), call(U1, T, X, F), formal(F, Z, A).
//        |
//        |% merge members pointing to the same obj:
//        |pt(A, B) :- member(X, F, A), member(X, F, B).
//        |pt(A, B) :- member(X, F, A), pt(X, Y), member(Y, F, B).
//        |
//        |% link scope to scope of closure
//        |parentscope(OUTER, INNER) :- functiondecl(OUTER, U, INNER).
//        |pt(A, B):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "closure", B).
//        |pt(A, B):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "local", B).
//        |
//        |%    TODO: the following would allow writes to be propagated back, but makes everything absolutely
//        |%    conservative by merging local and global scopes of all function that contain any other function decl.
//        |%    for now we rather have a policy against writing to outer environments
//        |% scope needs to be shared both directions, as inner functions can update values in outer scopes
//        |pt(B, A):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "closure", B).
//        |pt(B, A):-scope(F2, "closure", A),parentscope(F1,F2),scope(F1, "local", B).
//        |      """.stripMargin
//    datalog.loadRules(rules)
//
//
//
//
//
//    println(datalog.ruleStr + "%%%")
//
//    for ((function, summary) <- methodSummaries) {
//      val facts = summaryToDatalog(function, summary)
//      facts.foreach(datalog.load)
//      facts.map(println)
//      println("%")
//    }
//
//
//    datalog
//  }
//
//
//
//}
