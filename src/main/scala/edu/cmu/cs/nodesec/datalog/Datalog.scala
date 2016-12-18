package edu.cmu.cs.nodesec.datalog

import edu.cmu.cs.nodesec.analysis._
import za.co.wstoop.jatalog.{Jatalog, Rule, Expr => DLExpr}

import scala.collection.JavaConverters._

/**
  * Lightweight layer to abstract from concrete datalog engine
  *
  * Created by ckaestne on 11/25/16.
  */


sealed abstract class DRelation(fun: Fun) {
  def prefix = fun.uniqueId

  def predicate: String

  def terms: List[String]

  def toDatalogExpr: DLExpr = new DLExpr(predicate, terms.map('"' + _).asJava)

  override def toString: String = predicate + terms.map('"' + _ + '"').mkString("(", ", ", ").")
}

case class DFormal(fun: Fun, idx: Int, obj: Value) extends DRelation(fun) {
  def predicate = "formal"

  def terms = prefix :: Integer.toString(idx + 1) :: prefix + obj :: Nil
}

case class DReturn(fun: Fun, obj: Value) extends DRelation(fun) {
  def predicate = "return"

  def terms = prefix :: prefix + obj :: Nil
}

case class DMember(fun: Fun, obj: Value, field: String, fieldValue: Value) extends DRelation(fun) {
  def predicate = "member"

  def terms = prefix + obj :: field :: prefix + fieldValue :: Nil

}

case class DActual(fun: Fun, targetObj: Value, idx: Int, value: Value) extends DRelation(fun) {
  def predicate = "actual"

  def terms = prefix + targetObj :: Integer.toString(idx + 1) :: prefix + value :: Nil
}

case class DInvoke(fun: Fun, targetObj: Value, returnObj: Value) extends DRelation(fun) {
  def predicate = "invoke"

  def terms = prefix :: prefix + targetObj :: prefix + returnObj :: Nil
}

case class DFunctionDecl(fun: Fun, obj: Value, targetFun: Fun) extends DRelation(fun) {
  def predicate = "functiondecl"

  def terms = prefix :: prefix + obj :: targetFun.uniqueId :: Nil
}

case class DScope(fun: Fun, kind: String, scopeObj: Value) extends DRelation(fun) {
  def predicate = "scope"

  def terms = prefix :: kind :: prefix + scopeObj :: Nil
}

case class DClosureToLocal(fun: Fun, localVar: LocalVariable, innerFun: Fun, closureVar: ExternalVariable) extends DRelation(fun) {
  def predicate = "closure2local"

  def terms = prefix + localVar :: innerFun.uniqueId + closureVar :: Nil
}

case class DClosureToClosure(fun: Fun, innerFun: Fun, closureVar: ExternalVariable) extends DRelation(fun) {
  def predicate = "closure2closure"

  def terms = prefix + closureVar :: innerFun.uniqueId + closureVar :: Nil
}


case class DStore(fun: Fun, obj: Value, field: String, fieldValue: Value) extends DRelation(fun) {
  def predicate = "store"

  def terms = prefix + obj :: field :: prefix + fieldValue :: Nil
}

case class DLoad(fun: Fun, obj: Value, field: String, fieldValue: Value) extends DRelation(fun) {
  def predicate = "load"

  def terms = prefix + obj :: field :: prefix + fieldValue :: Nil
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
    rule.toString
  }

  def loadRules(rules: String) = {
    val r = rules.split("\n").filter(_.trim.nonEmpty).filterNot(_ startsWith "%")
    ruleStr += r.map(_ + "\n").mkString
    r.map(Jatalog.prepareStatement).foreach(_.execute(jatalog))
  }

  def load(facts: List[DRelation]): Unit = facts.foreach(load)

  def load(fact: DRelation): Unit = jatalog.fact(fact.toDatalogExpr)

  def query(predicate: String, terms: String*): Seq[Map[String, String]] =
    jatalog.query(new DLExpr(predicate, terms.map(t => if (t startsWith "\"") t.dropRight(1) else t).asJava)).
      asScala.toSeq.map(_.asScala.toMap.mapValues(s => if (s startsWith "\"") s + "\"" else s))
}

/** helper functions */
object Datalog {
  def stripQuotes(s: String): String = if (s startsWith "\"") s.drop(1).dropRight(1) else s

  def stripQuotes(r: Seq[Map[String, String]]): Seq[Map[String, String]] = r.map(_.mapValues(stripQuotes))

}