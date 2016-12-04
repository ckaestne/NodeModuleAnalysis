package edu.cmu.cs.nodesec.analysis


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
