package edu.cmu.cs.nodesec

case class Taint(n: String)

case class Value(var v: V, var taints: Set[Taint] = Set(), var members: Map[String, Value] = Map()) {
  def getField(name: String): Value =
    if (members.contains(name)) members(name)
    else {
      val field = new Value(new SymbolicV("field of " + v), this.taints)
      members = members + (name -> field)
      field
    }


  def assign(thatValue: Value): Value = {
    if (taints.nonEmpty)
      System.err.println("modifying value with taint "+taints)

    this.v = thatValue.v
    this.taints = thatValue.taints
    this
  }

}

trait V

case class Undefined() extends V

case class ConcreteV(v: String) extends V

case class SymbolicV(text: String = "") extends V

case class FunctionV(funExpr: FunExpr, closure: Option[Env]) extends V {
  override def toString() = "function" + funExpr.param.map(_.a).mkString("(", ",", ")")

  def call(env: Env, args: List[Value]): Value = {
    val env = Env.empty.copy(closure=closure)
    for ((p,a)<-funExpr.param zip args)
      env.setVar(p.a, a)
    funExpr.body.execute(env)
    env.returned.getOrElse(None).getOrElse(new Value(Undefined()))
  }
}


object Env {
  def empty = Env(Map(), Nil, None)
}

case class Env(var vars: Map[String, Value], var implicits: List[Set[Taint]], var returned: Option[Option[Value]], closure: Option[Env] = None) {
  def setVar(s: String, v: Value) = vars = vars + (s -> v)

  private def lookupIfDefined(name: String): Option[Value] = {
    val r = vars.get(name)
    if (!r.isDefined && closure.isDefined)
      closure.get.lookupIfDefined(name)
    else
      r
  }

  def lookup(name: String): Value = {
    val r = lookupIfDefined(name)
    if (r.isDefined) r.get
    else {
      System.err.println("undefined identifier "+name)
      val newVal = new Value(new SymbolicV("unknown " + name), Set())
      vars = vars + (name -> newVal)
      newVal
    }
  }

  def setReturn(retValue: Option[Value]): Unit = returned = Some(retValue)

  def hasReturned = returned.isDefined


  //recursively merge values, tracking both
  def join(newEnv: Env): Env = this // TODO


  def pushImplicits(taints: Set[Taint]): Env = {
    implicits = taints :: this.implicits;
    this
  }

  def popImplicits(): Env = {
    implicits = this.implicits.tail;
    this
  }

}
