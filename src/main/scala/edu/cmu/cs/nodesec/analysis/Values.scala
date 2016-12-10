package edu.cmu.cs.nodesec.analysis

/**
  * Created by ckaestne on 11/25/16.
  */

sealed trait Value {
  def isUnknown: Boolean = false
}

case class Constant(s: String) extends Obj

//primitive values include all nonobjects, including "undefined"
object PrimitiveValue extends Value {
  override def toString: String = "primitive-value"
}

class Obj(_name: String = NameHelper.genObjectName) extends Value {
  //name is for debugging only
  override def toString: String = _name
}

case class Param(paramName: String) extends Obj("param-" + paramName)

class FunctionValue(val f: Fun, _name: String = NameHelper.genFunctionName) extends Obj(_name)

class UnknownValue(_name: String = "unknown-" + NameHelper.genObjectName) extends Obj(_name) {
  override def isUnknown: Boolean = true
}

case class MethodReturnValue(target: Set[Value], thisObj: Set[Value], args: List[Set[Value]]) extends UnknownValue {
  override def toString: String = "ret-" + super.toString
}

class UnknownLoadValue(val stmt: Option[Load]) extends UnknownValue

