package edu.cmu.cs.nodesec.datalog

import edu.cmu.cs.nodesec.analysis._
import za.co.wstoop.jatalog.{Jatalog, Rule, Expr => DLExpr}

import scala.collection.JavaConverters._

/**
  * Lightweight layer to abstract from concrete datalog engine
  *
  * Created by ckaestne on 11/25/16.
  */


sealed abstract class DFact(fun: Fun) {
  def prefix = fun.uniqueId

  def predicate: String

  def terms: List[String]

  def toDatalogExpr: DLExpr = new DLExpr(predicate, terms.map('"' + _).asJava)

  override def toString: String = predicate + terms.map('"' + _ + '"').mkString("(", ", ", ").")
}

case class DFormal(fun: Fun, idx: Int, p: Variable) extends DFact(fun) {
  def predicate = "formal"

  def terms = prefix :: Integer.toString(idx + 1) :: prefix + p :: Nil
}

case class DAssign(fun: Fun, l: Variable, r: Variable) extends DFact(fun) {
  def predicate = "assign"

  def terms = prefix + l :: prefix + r :: Nil
}

case class DReturn(fun: Fun, obj: Value) extends DFact(fun) {
  def predicate = "return"

  def terms = prefix :: prefix + obj :: Nil
}

case class DMember(fun: Fun, obj: Value, field: String, fieldValue: Value) extends DFact(fun) {
  def predicate = "member"

  def terms = prefix + obj :: field :: prefix + fieldValue :: Nil

}

case class DActual(fun: Fun, targetObj: Variable, idx: Int, value: Variable) extends DFact(fun) {
  def predicate = "actual"

  def terms = prefix + targetObj :: Integer.toString(idx + 1) :: prefix + value :: Nil
}

case class DInvoke(fun: Fun, target: Variable, returnVar: Variable) extends DFact(fun) {
  def predicate = "invoke"

  def terms = prefix :: prefix + target :: prefix + returnVar :: Nil
}

case class DFunctionDecl(fun: Fun, v: Variable, targetFun: Fun) extends DFact(fun) {
  def predicate = "functiondecl"

  def terms = prefix :: prefix + v :: targetFun.uniqueId :: Nil
}

case class DScope(fun: Fun, kind: String, scopeObj: Value) extends DFact(fun) {
  def predicate = "scope"

  def terms = prefix :: kind :: prefix + scopeObj :: Nil
}

case class DClosureToLocal(fun: Fun, localVar: LocalVariable, innerFun: Fun, closureVar: ExternalVariable) extends DFact(fun) {
  def predicate = "closure2local"

  def terms = prefix + localVar :: innerFun.uniqueId + closureVar :: Nil
}

case class DClosureToClosure(fun: Fun, innerFun: Fun, closureVar: ExternalVariable) extends DFact(fun) {
  def predicate = "closure2closure"

  def terms = prefix + closureVar :: innerFun.uniqueId + closureVar :: Nil
}


case class DStore(fun: Fun, targetVar: Variable, field: String, sourceVar: Variable) extends DFact(fun) {
  def predicate = "store"

  def terms = prefix + targetVar :: field :: prefix + sourceVar :: Nil
}

case class DLoad(fun: Fun, targetVar: Variable, sourceVar: Variable, field: String) extends DFact(fun) {
  def predicate = "load"

  def terms = prefix + targetVar :: prefix + sourceVar :: field :: Nil
}


case class DStack(fun: Fun, v: Variable, o: Value) extends DFact(fun) {
  def predicate = "stack"

  def terms = prefix + v :: prefix + o :: Nil
}


case class Expr(predicate: String, terms: String*) {
  def toDatalog(): DLExpr = new DLExpr(predicate, terms.asJava)
}


class Datalog {


  import collection.JavaConverters._

  val jatalog = new Jatalog()

  var ruleStr = ""

  def rule(head: Expr, body: Expr*) = {
    val rule = new Rule(head.toDatalog(), body.map(_.toDatalog()): _*)
    jatalog.rule(rule)
    ruleStr += rule.toString + ".\n"
    rule.toString + "."
  }

  def loadRules(rules: String) = {
    val r = rules.split("\n").filter(_.trim.nonEmpty).filterNot(_ startsWith "%")
    ruleStr += r.map(_ + "\n").mkString
    r.map(Jatalog.prepareStatement).foreach(_.execute(jatalog))
    rules
  }

  def load(facts: Iterable[DFact]): Unit = facts.foreach(load)

  def load(fact: DFact): Unit = jatalog.fact(fact.toDatalogExpr)

  def query(predicate: String, terms: String*): Seq[Map[String, String]] = {
    println(s"$predicate(${terms.mkString(", ")})?")
    jatalog.query(new DLExpr(predicate, terms.map(t => if (t startsWith "\"") t.dropRight(1) else t).asJava)).
      asScala.toSeq.map(_.asScala.toMap.mapValues(s => if (s startsWith "\"") s + "\"" else s))
  }
}

/** helper functions */
object Datalog {
  def stripQuotes(s: String): String = if (s startsWith "\"") s.drop(1).dropRight(1) else s

  def stripQuotes(r: Seq[Map[String, String]]): Seq[Map[String, String]] = r.map(_.mapValues(stripQuotes))

}