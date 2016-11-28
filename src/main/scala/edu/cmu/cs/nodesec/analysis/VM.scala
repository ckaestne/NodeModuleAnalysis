package edu.cmu.cs.nodesec.analysis

import scala.util.parsing.input.Positional

object VariableHelper {
  def freshVar = new AnonymousVariable()

  val thisVar = new LocalVariable("this")
  val emptyStatement = Sequence(Nil)
}

trait Variable

/**
  * local variables are declared within the function
  */
case class LocalVariable(name: String) extends Variable {
  override def toString: String = "lv-" + name
}

/**
  * all named variables that are not declared come from an
  * outer scope (possibly global)
  */
case class ExternalVariable(name: String) extends Variable {
  override def toString: String = "ev-" + name
}

/**
  * anonymous variables are used in the analysis for temporary
  * computations
  */
class AnonymousVariable() extends Variable {
  override def toString: String = "v-" + this.hashCode()
}


trait Statement extends Positional {
  def ++(s: Statement) = s match {
    case EmptyStatement => this
    case Sequence(i) => Sequence(this :: i)
    case _ => Sequence(List(this, s))
  }

  def ++(s: Sequence) = Sequence(this :: s.s)

  override def equals(o: scala.Any): Boolean = o match {
    case that: Statement => this eq that
    case _ => false
  }

  def copyPosition(p: Positional): this.type = {
    this.setPos(p.pos)
    this
  }


}

case class Sequence(s: List[Statement]) extends Statement {
  override def ++(that: Statement) = that match {
    case EmptyStatement => this
    case Sequence(i) => Sequence(s ++ i)
    case _ => Sequence(s :+ that)
  }

  override def ++(that: Sequence) = Sequence(this.s ++ that.s)

}

object EmptyStatement extends Statement {
  override def ++(s: Statement) = s
}

case class Assignment(l: Variable, r: Variable) extends Statement

case class PrimAssignment(l: Variable) extends Statement

case class ConstAssignment(l: Variable, v: String) extends Statement

case class OpStatement(result: Variable, v1: Variable, v2: Variable) extends Statement

case class Return(l: Variable) extends Statement

case class Constructor(result: Variable, name: Variable, params: List[Variable]) extends Statement

case class Call(result: Variable, name: Variable, vthis: Variable, params: List[Variable]) extends Statement

case class Load(result: Variable, v: Variable, field: String) extends Statement

case class Store(target: Variable, field: String, v: Variable) extends Statement

case class FunDecl(v: Variable, args: List[LocalVariable], localVariables: List[LocalVariable], body: Statement) extends Statement {
  lazy val uniqueId = Integer.toHexString(hashCode()) + "#"
}

case class ConditionalStatement(alt1: Statement, alt2: Statement) extends Statement

case class LoopStatement(inner: Statement) extends Statement
