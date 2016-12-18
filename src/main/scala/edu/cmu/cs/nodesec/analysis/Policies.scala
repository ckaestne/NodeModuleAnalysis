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

  import Datalog.stripQuotes

  class ComposedPolicy(p1: Policy, p2: Policy) extends Policy {
    def apply(d: Datalog, f: Fun) = p1(d, f) ++ p2(d, f)
  }

  val noCallToRequire = new Policy {


    def apply(d: Datalog, fun: Fun) = {
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

  val noWriteToClosure = new Policy {
    override def apply(datalog: Datalog, f: Fun): Seq[PolicyViolation] = {
      val rules = f.allClosureVariables.map("closurevar(\"" + f.uniqueId + _ + "\").\n").mkString +
        "closurevar(V):-closurevar(X),closure2closure(X,V).\n" +
        "writeToClosure(V):-closurevar(V),assign(V,U)."
      println(datalog.loadRules(rules))

      val result = stripQuotes(datalog.query("writeToClosure", "X"))

      result.map(
        r => PolicyViolation(s"Assignment to nonlocal object found (${r("X")})", NoPosition)
      )
    }
  }


  val noStoreToClosure = new Policy {
    override def apply(datalog: Datalog, f: Fun): Seq[PolicyViolation] = {
      //TODO see whether this can be simplified; annonyingly difficult
      //because we may read from things that are not known objects on the
      //stack
      val rules = f.allClosureVariables.map("closurevar(\"" + f.uniqueId + _ + "\").\n").mkString +
        //        "closurevar(V):-closurevar(X),closure2closure(X,V).\n" +
        "varFromClosure(V):-closurevar(V).\n" +
        "varFromClosure(X):-assign(X,Y),varFromClosure(Y).\n" +
        "varFromClosure(X):-varFromClosure(Y),load(X,Y,F).\n" +
        "varFromClosure(X):-varFromClosure(Y),alias(X,Y).\n" +
        "alias(A,B):-stack(A,O),stack(B,O).\n" +
        "alias(A,B):-store(Z1,F,A),load(B,Z2,F),alias(Z1,Z2).\n" +
        "alias(A,B):-alias(B,A).\n" +
        "alias(A,B):-load(A,Z1,F),load(B,Z2,F),alias(Z1,Z2).\n" +
        "storeToClosure(V,F):-store(V,F,U), varFromClosure(V)."
      println(datalog.loadRules(rules))

      val result = stripQuotes(datalog.query("storeToClosure", "X", "F"))

      result.map(
        r => PolicyViolation(s"Store to nonlocal object found (${r("X")}.${r("F")})", NoPosition)
      )
    }
  }

  private val noReadFromClosureRules = "closurevar(V):-closurevar(X),closure2closure(X,V).\n" +
    "readFromClosure(V):-assign(X,V), closurevar(V).\n" +
    "readFromClosure(V):-store(X,F,V), closurevar(V).\n" +
    "readFromClosure(V):-invoke(U1,V,U2), closurevar(V).\n" +
    "readFromClosure(V):-actual(U1,U2,V), closurevar(V).\n" +
    "readFromClosure(V):-load(X,V,F), closurevar(V).\n"

  val noReadFromClosure = new Policy {
    override def apply(datalog: Datalog, f: Fun): Seq[PolicyViolation] = {
      val rules = f.allClosureVariables.map("closurevar(\"" + f.uniqueId + _ + "\").\n").mkString +
        noReadFromClosureRules
      println(datalog.loadRules(rules))
      val result = stripQuotes(datalog.query("readFromClosure", "F"))

      result.map(
        r => PolicyViolation(s"Read from closure/global object found (${r("F")})", NoPosition)
      )
    }
  }

  val noPrototype = new Policy {
    override def apply(datalog: Datalog, fun: Fun): Seq[PolicyViolation] = {
      datalog.loadRules(
        "accessToPrototype(X,Y) :- store(X, \"prototype\", Y).\n" +
          "accessToPrototype(X,Y) :- store(X, \"__proto__\", Y).\n" +
          "accessToPrototype(X,Y) :- load(X, Y, \"prototype\").\n" +
          "accessToPrototype(X,Y) :- load(X, Y, \"__proto__\").\n"
      )
      val result = stripQuotes(datalog.query("accessToPrototype", "X", "Y"))

      result.map(
        r => PolicyViolation(s"Access to prototype found (${r("X")}.prototype)", NoPosition)
      )
    }
  }

  val noForbiddenGlobalObjects = new Policy {
    //same strategy as noReadFromClosure
    override def apply(datalog: Datalog, f: Fun): Seq[PolicyViolation] = {
      val forbiddenClosureVariables = Set("eval", "arguments")
      val rules =
        forbiddenClosureVariables.map("forbiddenGlobal(\"" + f.uniqueId + new ExternalVariable(_) + "\").\n").mkString +
          f.allClosureVariables.map("closurevar(\"" + f.uniqueId + _ + "\").\n").mkString +
          noReadFromClosureRules +
          "accessToForbiddenGlobals(G):-readFromClosure(G),forbiddenGlobal(G)."
      println(datalog.loadRules(rules))
      val result = stripQuotes(datalog.query("accessToForbiddenGlobals", "F"))

      result.map(
        r => PolicyViolation(s"Access to forbidden global object `${r("F")}` found", NoPosition)
      )
    }

    //      override def apply(datalog: Datalog, mainFun: Fun): Seq[PolicyViolation] = {
    //        datalog.loadRules("readFromGlobal(G,F) :- load(G, F, U2).\n" +
    //          "readFromGlobal(G,F) :- pt(X, G), load(X, F, U2).\n" +
    //          "forbiddenGlobal(\"eval\"). \n forbiddenGlobal(\"arguments\").\n" +
    //          "accessToForbiddenGlobals(G, F) :- readFromGlobal(G, F), forbiddenGlobal(F)."
    //        )
    //        val result = stripQuotes(datalog.query("accessToForbiddenGlobals", "\"" + mainFun.uniqueId + globalObj + "\"", "F"))
    //
    //        result.map(
    //          r => PolicyViolation(s"Access to forbidden global object `${r("F")}` found", NoPosition)
    //        )
    //      }
  }

  val noAlwaysUnresolvedFunctionCalls = new Policy {
    //we cannot ask whether it is always resolved, only whether it is at least sometimes resolved
    //debugging rather than security check
    override def apply(datalog: Datalog, fun: Fun): Seq[PolicyViolation] = {
      datalog.loadRules("hasCall(X):-call(U1, U2, X, U3).\n" +
        "alwaysUnresolvedFunctionCalls(F, X) :- invoke(F, U3, X), not hasCall(X).")
      val result = stripQuotes(datalog.query("alwaysUnresolvedFunctionCalls", "F", "X"))

      result.map(
        r => PolicyViolation(s"Call to function never resolved (${r("X")})", getFunctionCallPositionByRetObj(r("X"), fun))
      )
    }
  }

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
