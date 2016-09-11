package edu.cmu.cs.nodesec


trait Stmt {
  def execute(env: Env)
}

case class ExpressionStmt(expr: Expr) extends Stmt {
  override def execute(env: Env) = expr.eval(env)
}

case class WhileStmt(expr: Expr, body: Stmt) extends Stmt {
  override def execute(env: Env) = {
    val v = expr.eval(env)
    val innerEnv = env.copy()
    innerEnv.pushImplicits(v.taints)
    body.execute(innerEnv)
    innerEnv.popImplicits
    env.join(env)
  }
}

case class IfStmt(expr: Expr, t: Stmt, e: Option[Stmt]) extends Stmt {
  override def execute(env: Env) = {
    val v = expr.eval(env)
    val innerEnv = env.copy()
    innerEnv.pushImplicits(v.taints)
    t.execute(innerEnv)
    innerEnv.popImplicits
    //else
    if (e.isDefined) {
      env.pushImplicits(v.taints)
      e.get.execute(env)
      env.popImplicits
    }
    env.join(env)
  }
}


case class ReturnStmt(expr: Option[Expr]) extends Stmt {
  override def execute(env: Env) = {
    if (expr.isDefined) {
      val v = expr.get.eval(env)
      env.setReturn(Some(v))
    } else
      env.setReturn(None)
  }
}

case class CompoundStmt(inner: List[Stmt]) extends Stmt {
  override def execute(env: Env) = {
    for (stmt <- inner; if !env.hasReturned)
      stmt.execute(env)
  }
}

case class VarStmt(vars: List[VarDef]) extends Stmt {
  override def execute(env: Env) = for (v <- vars) {
    val init = v.init.map(_.eval(env)).getOrElse(new Value(Undefined()))
    env.setVar(v.name.a, init)
  }
}

case class EmptyStmt() extends Stmt {
  override def execute(env: Env) {}
}


case class NotImplStmt(inner: Any) extends Stmt {
  override def execute(env: Env) = ???
}

case class VarDef(name: Id, init: Option[Expr])


trait Expr {
  def eval(env: Env): Value
}

case class BinExpr(a: Expr, op: String, b: Expr) extends Expr {
  override def eval(env: Env): Value = {
    val aval = a.eval(env)
    val bval = b.eval(env)
    (aval.v, bval.v) match {
      case (ConcreteV(a), ConcreteV(b)) => new Value(ConcreteV(a + op + b), aval.taints ++ bval.taints)
      case _ => new Value(SymbolicV(aval.v + op + bval.v), aval.taints ++ bval.taints)
    }
  }
}


case class AssignExpr(a: Expr, op: String, b: Expr) extends Expr {
  override def eval(env: Env): Value = {
    val aval = a.eval(env)
    val bval = b.eval(env)
    aval.assign(bval, env.implicitTaints)
  }
}

case class ITEExpr(i: Expr, t: Expr, e: Expr) extends Expr {
  override def eval(env: Env): Value = {
    //TODO fix environments
    val ival = i.eval(env)
    val innerEnv = env.copy()
    innerEnv.pushImplicits(ival.taints)
    val tval = t.eval(innerEnv)
    innerEnv.popImplicits
    //else
    env.pushImplicits(ival.taints)
    val eval = e.eval(env)
    env.popImplicits
    env.join(env)

    (ival.v, tval.v, eval.v) match {
      case (ConcreteV(a), ConcreteV(b), ConcreteV(c)) => new Value(ConcreteV(a + "?" + b + ":" + c), ival.taints ++ tval.taints ++ eval.taints)
      case _ => new Value(SymbolicV(ival.v + "?" + tval.v + ":" + eval.v), ival.taints ++ tval.taints ++ eval.taints)
    }
  }
}

case class PostExpr(a: Expr, s: String) extends Expr {
  override def eval(env: Env): Value = ???
}


case class UnaryExpr(a: String, e: Expr) extends Expr {
  override def eval(env: Env): Value = {
    val eval = e.eval(env)
    eval.v match {
      case ConcreteV(e) => new Value(ConcreteV(a + " " + e), eval.taints)
      case _ => new Value(SymbolicV(a + " " + eval.v), eval.taints)
    }
  }
}

case class FieldAcc(a: Expr, field: Id) extends Expr {
  override def eval(env: Env): Value = {
    val aval = a.eval(env)
    aval.getField(field.a)
  }
}

case class FunCall(a: Expr, args: List[Expr]) extends Expr {
  override def eval(env: Env): Value = {
    val aval = a.eval(env)
    val argval = args.map(_.eval(env))

    lazy val argsTaints = argval.foldLeft(Set[Taint]())((a, arg) => a ++ arg.taints)
    lazy val allTaints = aval.taints ++ argsTaints
    aval.v match {
      case f: FunctionV => f.call(env, argval)
      case Undefined() => new Value(Undefined())
      case s: SymbolicV =>
        System.err.println("function call with taints " + argsTaints + " to target with taints " + aval.taints)
        new Value(new SymbolicV("call on " + s), allTaints)
      case s: ConcreteV =>
        ???
    }
  }
}

case class NewExpr(a: Expr, args: List[Expr]) extends Expr {
  override def eval(env: Env): Value = {
    val aval = a.eval(env)
    val argval = args.map(_.eval(env))

    lazy val allTaints = argval.foldLeft(aval.taints)((a, arg) => a ++ arg.taints)
    aval.v match {
      case FunctionV(fun, closure) => ???
      case Undefined() => new Value(Undefined())
      case s: SymbolicV =>
        new Value(new SymbolicV("call on " + s), allTaints)
      case s: ConcreteV =>
        ???
    }
  }
}


case class FunExpr(name: Option[Id], param: List[Id], body: Stmt) extends Expr {
  override def eval(env: Env): Value = new Value(FunctionV(this, Some(env)))
}

case class Lit(a: String) extends Expr {
  override def eval(env: Env): Value = new Value(ConcreteV(a), Set())
}

case class Id(a: String) extends Expr {
  override def eval(env: Env): Value = env.lookup(a)
}

case class ConstExpr(a: String) extends Expr {
  override def eval(env: Env): Value = new Value(ConcreteV(a), Set())
}

case class ObjExpr(m: List[(String, Expr)]) extends Expr {
  override def eval(env: Env): Value = {
    val v = new Value(Undefined(), env.implicitTaints)
    for ((f, e) <- m) {
      val fv = e.eval(env)
      v.getField(f).assign(fv, env.implicitTaints)
    }
    v
  }
}


case class NotImplExpr(a: Any) extends Expr {
  override def eval(env: Env): Value = ???
}