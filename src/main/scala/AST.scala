package edu.cmu.cs.nodesec

import VariableHelper._

sealed trait AST extends Product {
  override def equals(that: Any) = that match {
    case thatRef: AnyRef => this eq thatRef
    case _ => false
  }
}

trait Stmt extends AST {

  def toVM(): Statement

}

case class ExpressionStmt(expr: Expr) extends Stmt {
  def toVM(): Statement = expr.toVM._1
}

case class WhileStmt(expr: Expr, body: Stmt) extends Stmt {
  def toVM(): Statement = body.toVM() ++ expr.toVM._1
}

case class IfStmt(expr: Expr, t: Stmt, e: Option[Stmt]) extends Stmt {
  def toVM(): Statement =
    ConditionalStatement(e.map(s => s.toVM()).getOrElse(emptyStatement), t.toVM()) ++
      expr.toVM._1
}


case class ReturnStmt(expr: Option[Expr]) extends Stmt {
  private def emptyReturn = {
    val r = freshVar
    (PrimAssignment(r), r)
  }

  def toVM(): Statement = {
    val (s, v) = expr.map(_.toVM).getOrElse(emptyReturn)
    Return(v) ++ s
  }
}

case class CompoundStmt(inner: List[Stmt]) extends Stmt {
  def toVM(): Statement =
    inner.map(_.toVM).reverse.fold(emptyStatement)(_ ++ _) ++
      inner.map(findVars).fold(emptyStatement)(_ ++ _)

  private def findVars(s: Stmt): Statement = s match {
    case VarStmt(vars) => vars.map(v => PrimAssignment(new NamedVariable(v.name.a))).fold(emptyStatement)(_ ++ _)
    case CompoundStmt(inner) => inner.map(findVars).fold(emptyStatement)(_ ++ _)
    case IfStmt(_, t, e) => findVars(t) ++ e.map(findVars).getOrElse(emptyStatement)
    case WhileStmt(_, b) => findVars(b)
    case _ => emptyStatement
  }
}

case class VarStmt(vars: List[VarDef]) extends Stmt {

  private def toDefStmt(n: String, e: Expr): Statement = {
    val (s, v) = e.toVM
    Assignment(new NamedVariable(n), v) ++ s
  }

  def toVM(): Statement = vars.filter(_.init.isDefined).map(x => toDefStmt(x.name.a, x.init.get)).fold(emptyStatement)(_ ++ _)
}

case class EmptyStmt() extends Stmt {
  def toVM(): Statement = emptyStatement
}


case class NotImplStmt(inner: Any) extends Stmt {
  def toVM(): Statement = ???
}

case class VarDef(name: Id, init: Option[Expr])


trait Expr extends AST {

  def toVM: (Statement, Variable)

  //  def eval(env: Env): Value
}

case class BinExpr(a: Expr, op: String, b: Expr) extends Expr {

  def toVM: (Statement, Variable) = {
    var (s1, v1) = a.toVM
    var (s2, v2) = b.toVM
    var r = freshVar
    (OpStatement(r, v1, v2) ++ s1 ++ s2, r)
  }

}


case class AssignExpr(a: Expr, op: String, b: Expr) extends Expr {

  def toVM: (Statement, Variable) =
    a match {
      case FieldAcc(targ, field) =>
        val (s1, v1) = targ.toVM
        val (s2, v2) = b.toVM
        //TODO look up evaluation order
        (Store(v1, field.a, v2) ++ s1 ++ s2, v2)
      case _ =>
        val (s1, v1) = a.toVM
        val (s2, v2) = b.toVM
        //TODO look up evaluation order
        (Assignment(v1, v2) ++ s1 ++ s2, v1)

    }


}

case class ITEExpr(i: Expr, t: Expr, e: Expr) extends Expr {
  def toVM: (Statement, Variable) = {
    val (si, vi) = i.toVM
    val (st, vt) = t.toVM
    val (se, ve) = e.toVM
    val r = freshVar
    (ConditionalStatement(Assignment(r, ve) ++ se, Assignment(r, vt) ++ st) ++ si, r)
  }


}

case class PostExpr(a: Expr, s: String) extends Expr {
  def toVM: (Statement, Variable) = a.toVM
}


case class UnaryExpr(a: String, e: Expr) extends Expr {
  def toVM: (Statement, Variable) = if (Set("+", "++", "-", "--", "!", "typeof") contains a) e.toVM else ???

}

case class FieldAcc(a: Expr, field: Id) extends Expr {
  def toVM: (Statement, Variable) = {
    val (s, v) = a.toVM
    val r = freshVar
    (Load(r, v, field.a) ++ s, r)
  }
}

case class FunCall(a: Expr, args: List[Expr]) extends Expr {
  def toVM: (Statement, Variable) = {
    val (fs, fv) = a.toVM
    val argV = args.map(_.toVM)
    val argStmts: Statement = argV.map(_._1).foldRight(emptyStatement)(_ ++ _)

    val r = freshVar
    (Call(r, fv, thisVar, argV.map(_._2)) ++ argStmts ++ fs, r)
  }
}

case class NewExpr(a: Expr, args: List[Expr]) extends Expr {
  def toVM: (Statement, Variable) = {
    val (fs, fv) = a.toVM
    val argV = args.map(_.toVM)
    val argStmts: Statement = argV.map(_._1).foldRight(emptyStatement)(_ ++ _)

    val r = freshVar
    (Constructor(r, fv, argV.map(_._2)) ++ argStmts ++ fs, r)
  }
}


case class FunExpr(name: Option[Id], param: List[Id], body: Stmt) extends Expr {
  def toVM: (Statement, Variable) = {
    val funr = name.map(x => new NamedVariable(x.a)).getOrElse(freshVar)
    (FunDecl(funr, param.map(x => new NamedVariable(x.a)), body.toVM), funr)
  }
}


case class Lit(a: String) extends Expr {
  def toVM: (Statement, Variable) = {
    val r = freshVar
    (ConstAssignment(r, a), r)
  }
}

case class Id(a: String) extends Expr {
  def toVM: (Statement, Variable) = (emptyStatement, new NamedVariable(a))
}

case class ConstExpr(a: String) extends Expr {
  def toVM: (Statement, Variable) = {
    val r = freshVar
    (PrimAssignment(r), r)
  }
}

case class ObjExpr(m: List[(String, Expr)]) extends Expr {
  def toVM: (Statement, Variable) = {
    val r = freshVar
    var result: Statement = Constructor(r, new ConstString("Object"), Nil)
    for (field <- m) {
      val (s, v) = field._2.toVM
      result = Store(r, field._1, v) ++ s ++ result
    }

    (result, r)
  }
}


case class NotImplExpr(a: Any) extends Expr {
  def toVM: (Statement, Variable) = ???
}