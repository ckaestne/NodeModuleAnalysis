package edu.cmu.cs.nodesec.datalog

import edu.cmu.cs.nodesec.analysis._
import za.co.wstoop.jatalog.{Jatalog, Rule, Expr => DLExpr}

import scala.collection.JavaConverters._

/**
  * Lightweight layer to abstract from concrete datalog engine
  *
  * Created by ckaestne on 11/25/16.
  */


sealed abstract class DRelation(fun: FunDecl) {
  def prefix = fun.uniqueId

  def predicate: String

  def terms: List[String]

  def toDatalogExpr: DLExpr = new DLExpr(predicate, terms.asJava)

  override def toString: String = predicate + terms.mkString("(", ", ", ").")
}

case class DFormal(fun: FunDecl, idx: Int, obj: Value) extends DRelation(fun) {
  def predicate = "formal"

  def terms = prefix :: Integer.toString(idx + 1) :: prefix + obj :: Nil
}

case class DReturn(fun: FunDecl, obj: Value) extends DRelation(fun) {
  def predicate = "return"

  def terms = prefix :: prefix + obj :: Nil
}

case class DMember(fun: FunDecl, obj: Value, field: String, fieldValue: Value) extends DRelation(fun) {
  def predicate = "member"

  def terms = prefix + obj :: field :: prefix + fieldValue :: Nil

}

case class DActual(fun: FunDecl, targetObj: Value, idx: Int, value: Value) extends DRelation(fun) {
  def predicate = "actual"

  def terms = prefix + targetObj :: Integer.toString(idx + 1) :: prefix + value :: Nil
}

case class DInvoke(fun: FunDecl, targetObj: Value, returnObj: Value) extends DRelation(fun) {
  def predicate = "invoke"

  def terms = prefix + targetObj :: prefix + returnObj :: Nil
}

case class DFunctionPtr(fun: FunDecl, obj: Value, targetFun: FunDecl) extends DRelation(fun) {
  def predicate = "functionptr"

  def terms = prefix + obj :: targetFun.uniqueId :: Nil
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
  }

  def loadRules(rules: String) = {
    val statements = rules.split("\n").filter(_.trim.nonEmpty).map(Jatalog.prepareStatement)
    statements.foreach(_.execute(jatalog))
  }

  def load(facts: List[DRelation]): Unit = facts.foreach(load)

  def load(fact: DRelation): Unit = jatalog.fact(fact.toDatalogExpr)

  def query(predicate: String, terms: String*): Seq[Map[String, String]] =
    jatalog.query(new DLExpr(predicate, terms: _*)).asScala.toSeq.map(Map() ++ _.asScala)
}