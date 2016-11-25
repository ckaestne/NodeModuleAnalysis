package edu.cmu.cs.nodesec.analysis

object VariableHelper {
  def freshVar = new AnonymousVariable()

  val thisVar = new NamedVariable("this")
  val emptyStatement = Sequence(Nil)
}

trait Variable

case class NamedVariable(name: String) extends Variable {
  override def toString: String = "var-" + name
}

class AnonymousVariable() extends Variable {
  override def toString: String = "v-" + this.hashCode()
}

class ConstString(a: String) extends Variable {
  override def toString: String = "\"" + a + "\""
}


trait Statement {
  def ++(s: Statement) = s match {
    case Sequence(i) => Sequence(this :: i)
    case _ => Sequence(List(this, s))
  }

  def ++(s: Sequence) = Sequence(this :: s.s)

  override def equals(o: scala.Any): Boolean = o match {
    case that: Statement => this eq that
    case _ => false
  }

}

case class Sequence(s: List[Statement]) extends Statement {
  override def ++(that: Statement) = that match {
    case Sequence(i) => Sequence(s ++ i)
    case _ => Sequence(s :+ that)
  }

  override def ++(that: Sequence) = Sequence(this.s ++ that.s)

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

case class FunDecl(v: Variable, args: List[NamedVariable], body: Statement) extends Statement

case class ConditionalStatement(alt1: Statement, alt2: Statement) extends Statement

case class LoopStatement(inner: Statement) extends Statement