package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.datalog.Datalog

import scala.util.parsing.input.{NoPosition, Position}

case class PolicyViolation(msg: String, pos: Position) {
  def render: String = {
    if (pos == NoPosition) msg
    else msg + " in line " + pos.line + "\n" + pos.longString
  }
}


trait Policy {
  def apply(datalog: Datalog, fun: Fun): Seq[PolicyViolation]

  def +(that: Policy): Policy = new Policies.ComposedPolicy(this, that)
}

/**
  * Created by ckaestne on 12/11/16.
  */
object Policies {


  class ComposedPolicy(p1: Policy, p2: Policy) extends Policy {
    def apply(d: Datalog, f: Fun) = p1(d, f) ++ p2(d, f)
  }

  val noCallToRequire = new Policy {


    def apply(d: Datalog, fun: Fun) = {
      import Datalog.stripQuotes
      val rules =
        "%% query\n" +
          "forbiddenCallTarget(\"require-obj\").\n" +
          "stack(\"" + fun.uniqueId + "lv-require\", \"require-obj\").\n" +
          "callToForbiddenTarget(RV) :- invoke(F, TV, RV), stack(TV, O), forbiddenCallTarget(O)."
      println(d.loadRules(rules))
      val result = stripQuotes(d.query("callToForbiddenTarget", "X"))

      result.map(
        r => PolicyViolation("Potential call to 'require' found", getFunctionCallPositionByRetObj(r("X"), fun))
      )
    }


  }


  //
  //  val noWriteToClosure = new Policy {
  //    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, MethodSummary)]): Seq[PolicyViolation] = {
  //      datalog.loadRules("reaches(A,B) :- member(A,F,B).\n" +
  //        "reaches(A,B) :- member(A,F,C), reaches(C,B).\n" +
  //        "writeToClosure(W,F) :- store(W, F, U2), reaches(O, W), scope(U3, \"closure\", O).\n" +
  //        "writeToClosure(W,F) :- store(W, F, U2), scope(U3, \"closure\", W)."
  //      )
  //      val result = stripQuotes(datalog.query("writeToClosure", "X", "F"))
  //
  //      result.map(
  //        r => PolicyViolation(s"Write to nonlocal object found (${r("X")}.${r("F")})", NoPosition)
  //      )
  //    }
  //  }
  //
  //  val noReadFromGlobal = new Policy {
  //    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, MethodSummary)]): Seq[PolicyViolation] = {
  //      val globalObj = methodSummaries.find(_._1 == mainFun).get._2.closureObj
  //      datalog.loadRules("readFromGlobal(G,F) :- load(G, F, U2).\n" +
  //        "readFromGlobal(G,F) :- pt(X, G), load(X, F, U2)."
  //      )
  //      val result = stripQuotes(datalog.query("readFromGlobal", "\"" + mainFun.uniqueId + globalObj + "\"", "F"))
  //
  //      result.map(
  //        r => PolicyViolation(s"Read from global object found (${r("F")})", NoPosition)
  //      )
  //    }
  //  }
  //
  //  val noPrototype = new Policy {
  //    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, MethodSummary)]): Seq[PolicyViolation] = {
  //      datalog.loadRules("accessToPrototype(X,Y) :- member(X, \"prototype\", Y).\n" +
  //        "accessToPrototype(X,Y) :- member(X, \"__proto__\", Y)."
  //      )
  //      val result = stripQuotes(datalog.query("accessToPrototype", "X", "Y"))
  //
  //      result.map(
  //        r => PolicyViolation(s"Access to prototype found (${r("X")}.prototype)", NoPosition)
  //      )
  //    }
  //  }
  //
  //  val noForbiddenGlobalObjects = new Policy {
  //    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, MethodSummary)]): Seq[PolicyViolation] = {
  //      val globalObj = methodSummaries.find(_._1 == mainFun).get._2.closureObj
  //      datalog.loadRules("readFromGlobal(G,F) :- load(G, F, U2).\n" +
  //        "readFromGlobal(G,F) :- pt(X, G), load(X, F, U2).\n" +
  //        "forbiddenGlobal(\"eval\"). \n forbiddenGlobal(\"arguments\").\n" +
  //        "accessToForbiddenGlobals(G, F) :- readFromGlobal(G, F), forbiddenGlobal(F)."
  //      )
  //      val result = stripQuotes(datalog.query("accessToForbiddenGlobals", "\"" + mainFun.uniqueId + globalObj + "\"", "F"))
  //
  //      result.map(
  //        r => PolicyViolation(s"Access to forbidden global object `${r("F")}` found", NoPosition)
  //      )
  //    }
  //  }
  //
  //  val noAlwaysUnresolvedFunctionCalls = new Policy {
  //    //we cannot ask whether it is always resolved, only whether it is at least sometimes resolved
  //    //debugging rather than security check
  //    override def apply(datalog: Datalog, mainFun: Fun, methodSummaries: Set[(Fun, MethodSummary)]): Seq[PolicyViolation] = {
  //      val globalObj = methodSummaries.find(_._1 == mainFun).get._2.closureObj
  //      datalog.loadRules("hasCall(X):-call(U1, U2, X, U3).\n" +
  //        "alwaysUnresolvedFunctionCalls(F, X) :- invoke(F, U3, X), not hasCall(X).")
  //      val result = stripQuotes(datalog.query("alwaysUnresolvedFunctionCalls", "F", "X"))
  //
  //      result.map(
  //        r => PolicyViolation(s"Call to function never resolved (${r("X")})", getFunctionCallPositionByRetObj(r("X"), methodSummaries))
  //      )
  //    }
  //  }

  def allPolicies =
  //    noAlwaysUnresolvedFunctionCalls +
  //    noForbiddenGlobalObjects +
  //    noPrototype +
  //    noReadFromGlobal +
  //    noWriteToClosure +
    noCallToRequire


  private def getFunctionCallPositionByRetObj(retObjString: String, fun: Fun): Position = {
    val f = (fun.allInnerFunctions + fun).find(retObjString startsWith _.uniqueId)
    if (f.isDefined)
      f.get.body.nodes.flatMap(_.s).
        collect({ case c@Call(r, _, _, _) if fun.uniqueId + r == retObjString => c }).
        headOption.map(_.pos).getOrElse(NoPosition)
    else NoPosition
  }

}
