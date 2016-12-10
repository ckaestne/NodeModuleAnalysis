package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.datalog.{Datalog, _}
import edu.cmu.cs.nodesec.parser.FunctionBody

import scala.util.parsing.input.{NoPosition, Position}

/**
  * Created by ckaestne on 11/25/16.
  */
object MethodCompositionAnalysis {

  case class PolicyViolation(msg: String, pos: Position) {
    def render: String = {
      if (pos == NoPosition) msg
      else msg + " in line " + pos.line + "\n" + pos.longString
    }
  }

  trait Policy {
    def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, Env)]): Seq[PolicyViolation]

    def +(that: Policy): Policy = new ComposedPolicy(this, that)
  }


  class ComposedPolicy(p1: Policy, p2: Policy) extends Policy {
    def apply(d: Datalog, f: Fun, s: Set[(Fun, Env)]) = p1(d, f, s) ++ p2(d, f, s)
  }

  class NoCallToRequire extends Policy {


    def apply(d: Datalog, f: Fun, methodSummaries: Set[(Fun, Env)]) = {
      println(d.rule(Expr("callToRequire", "X", "REQOBJ"), /*:-*/ Expr("invoke", "F", "A", "X"), Expr("pt", "A", "REQOBJ")))
      val result = stripQuotes(d.query("callToRequire", "X", "\"" + f.uniqueId + "param-require\""))

      result.map(
        r => PolicyViolation("Potential call to 'require' found", getFunctionCallPositionByRetObj(stripQuotes(r("X")), methodSummaries))
      )
    }


  }

  private def stripQuotes(s: String): String = if (s startsWith "\"") s.drop(1).dropRight(1) else s

  private def stripQuotes(r: Seq[Map[String, String]]): Seq[Map[String, String]] = r.map(_.mapValues(stripQuotes))

  private def getFunctionCallPositionByRetObj(retObjString: String, methodSummaries: Set[(Fun, Env)]): Position = {
    for ((fun, env) <- methodSummaries
         if retObjString startsWith fun.uniqueId;
         (call, retVals) <- env.calls;
         retVal <- retVals
         if (fun.uniqueId + retVal) == retObjString
    ) return call.pos
    return NoPosition
  }

  lazy val noCallToRequire = new NoCallToRequire

  val noWriteToClosure = new Policy {
    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, Env)]): Seq[PolicyViolation] = {
      datalog.loadRules("reaches(A,B) :- member(A,F,B).\n" +
        "reaches(A,B) :- member(A,F,C), reaches(C,B).\n" +
        "writeToClosure(W,F) :- store(W, F, U2), reaches(O, W), scope(U3, \"closure\", O).\n" +
        "writeToClosure(W,F) :- store(W, F, U2), scope(U3, \"closure\", W)."
      )
      val result = stripQuotes(datalog.query("writeToClosure", "X", "F"))

      result.map(
        r => PolicyViolation(s"Write to nonlocal object found (${r("X")}.${r("F")})", NoPosition)
      )
    }
  }

  val noReadFromGlobal = new Policy {
    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, Env)]): Seq[PolicyViolation] = {
      val globalObj = methodSummaries.find(_._1 == mainFun).get._2.closureObj
      datalog.loadRules("readFromGlobal(G,F) :- load(G, F, U2).\n" +
        "readFromGlobal(G,F) :- pt(X, G), load(X, F, U2)."
      )
      val result = stripQuotes(datalog.query("readFromGlobal", "\"" + mainFun.uniqueId + globalObj + "\"", "F"))

      result.map(
        r => PolicyViolation(s"Read from global object found (${r("F")})", NoPosition)
      )
    }
  }

  val noPrototype = new Policy {
    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, Env)]): Seq[PolicyViolation] = {
      datalog.loadRules("accessToPrototype(X,Y) :- member(X, \"prototype\", Y).\n" +
        "accessToPrototype(X,Y) :- member(X, \"__proto__\", Y)."
      )
      val result = stripQuotes(datalog.query("accessToPrototype", "X", "Y"))

      result.map(
        r => PolicyViolation(s"Access to prototype found (${r("X")}.prototype)", NoPosition)
      )
    }
  }

  val noForbiddenGlobalObjects = new Policy {
    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, Env)]): Seq[PolicyViolation] = {
      val globalObj = methodSummaries.find(_._1 == mainFun).get._2.closureObj
      datalog.loadRules("readFromGlobal(G,F) :- load(G, F, U2).\n" +
        "readFromGlobal(G,F) :- pt(X, G), load(X, F, U2).\n" +
        "forbiddenGlobal(\"eval\"). \n forbiddenGlobal(\"arguments\").\n" +
        "accessToForbiddenGlobals(G, F) :- readFromGlobal(G, F), forbiddenGlobal(F)."
      )
      val result = stripQuotes(datalog.query("accessToForbiddenGlobals", "\"" + mainFun.uniqueId + globalObj + "\"", "F"))

      result.map(
        r => PolicyViolation(s"Access to forbidden global object `${r("F")}` found", NoPosition)
      )
    }
  }

  val noAlwaysUnresolvedFunctionCalls = new Policy {
    //we cannot ask whether it is always resolved, only whether it is at least sometimes resolved
    //debugging rather than security check
    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, Env)]): Seq[PolicyViolation] = {
      val globalObj = methodSummaries.find(_._1 == mainFun).get._2.closureObj
      datalog.loadRules("hasCall(X):-call(U1, U2, X, U3).\n" +
        "alwaysUnresolvedFunctionCalls(F, X) :- invoke(F, U3, X), not hasCall(X).")
      val result = stripQuotes(datalog.query("alwaysUnresolvedFunctionCalls", "F", "X"))

      result.map(
        r => PolicyViolation(s"Call to function never resolved (${r("X")})", getFunctionCallPositionByRetObj(r("X"), methodSummaries))
      )
    }
  }

  def allPolicies = noAlwaysUnresolvedFunctionCalls +
    noForbiddenGlobalObjects +
    noPrototype +
    noReadFromGlobal +
    noWriteToClosure +
    noCallToRequire

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
  def compose(methodSummaries: Set[(Fun, Env)]): Datalog = {

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

  def summaryToDatalog(fun: Fun, env: Env): List[DRelation] = {
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
