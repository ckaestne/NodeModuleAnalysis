package edu.cmu.cs.nodesec.parser

import edu.cmu.cs.nodesec.analysis.VariableHelper._
import edu.cmu.cs.nodesec.analysis._

import scala.util.parsing.input.Positional

sealed trait AST extends Product with Positional {
  override def equals(that: Any) = that match {
    case thatRef: AnyRef => this eq thatRef
    case _ => false
  }
}


trait Stmt extends AST {

  def toVM(isLocal: String => Boolean): Statement

  def getVarDecl: List[VarDef] = Nil

  def getInnerStmt: List[Stmt] = Nil

  def getFunDecl: List[FunDeclaration] = Nil

}

case class ExpressionStmt(expr: Expr) extends Stmt {
  def toVM(isLocal: String => Boolean): Statement = expr.toVM(isLocal)._1
}

case class WhileStmt(expr: Expr, body: Stmt) extends Stmt {
  def toVM(isLocal: String => Boolean): Statement = LoopStatement(body.toVM(isLocal)).copyPosition(this) ++ expr.toVM(isLocal)._1

  override def getInnerStmt: List[Stmt] = List(body)
}

case class DoWhileStmt(body: Stmt, expr: Expr) extends Stmt {
  def toVM(isLocal: String => Boolean): Statement =
    LoopStatement(expr.toVM(isLocal)._1 ++ body.toVM(isLocal)).copyPosition(this)

  override def getInnerStmt: List[Stmt] = List(body)
}

case class IfStmt(expr: Expr, t: Stmt, e: Option[Stmt]) extends Stmt {
  def toVM(isLocal: String => Boolean): Statement =
    ConditionalStatement(e.map(s => s.toVM(isLocal)).getOrElse(emptyStatement), t.toVM(isLocal)).copyPosition(this) ++
      expr.toVM(isLocal)._1

  override def getInnerStmt: List[Stmt] = t :: e.map(List(_)).getOrElse(Nil)
}

/**
  * vardecl does not contain an initializer; that is placed in init
  */
case class ForStmt(varDecl: Option[VarDef], init: Option[Expr], test: Option[Expr], update: Option[Expr], body: Stmt) extends Stmt {
  def toVM(isLocal: String => Boolean): Statement = {
    val initVm: Statement = init.map(_.toVM(isLocal)._1).getOrElse(EmptyStatement)
    val testVm: Statement = init.map(_.toVM(isLocal)._1).getOrElse(EmptyStatement)
    val updateVm: Statement = init.map(_.toVM(isLocal)._1).getOrElse(EmptyStatement)
    LoopStatement(body.toVM(isLocal) ++ updateVm ++ testVm).copyPosition(this) ++ initVm
  }

  override def getInnerStmt: List[Stmt] = List(body)

  override def getVarDecl: List[VarDef] = varDecl.map(List(_)).getOrElse(Nil)
}

case class ForInStmt(varDecl: Option[VarDef], left: Expr, right: Expr, body: Stmt, each: Boolean) extends Stmt {
  def toVM(isLocal: String => Boolean): Statement = {
    val leftVm = left.toVM(isLocal)
    val rightVm = right.toVM(isLocal)
    val load = Load(leftVm._2, rightVm._2, DynFieldAcc.magicDynFieldAccess)
    LoopStatement(body.toVM(isLocal) ++ load).copyPosition(this) ++ leftVm._1 ++ rightVm._1//TODO order may not be precise
  }

  override def getInnerStmt: List[Stmt] = List(body)

  override def getVarDecl: List[VarDef] = varDecl.map(List(_)).getOrElse(Nil)
}


case class ReturnStmt(expr: Option[Expr]) extends Stmt {
  private def emptyReturn = {
    val r = freshVar
    (PrimAssignment(r).copyPosition(this), r)
  }

  def toVM(isLocal: String => Boolean): Statement = {
    val (s, v) = expr.map(_.toVM(isLocal)).getOrElse(emptyReturn)
    Return(v).copyPosition(this) ++ s
  }
}

case class FunctionBody(inner: List[Stmt]) extends AST {
  def toVM(isLocal: String => Boolean): Statement =
    inner.map(_.toVM(isLocal)).reverse.fold(emptyStatement)(_ ++ _)

  lazy val localVars = inner.map(findLocalVar).fold(List[LocalVariable]())(_ ++ _)

  lazy val functionDeclarations = inner.map(findFunctionDeclarations).fold(List[LocalVariable]())(_ ++ _)


  private def findLocalVar(s: Stmt): List[LocalVariable] =
    s.getInnerStmt.map(findLocalVar).fold(s.getVarDecl.map(v => new LocalVariable(v.name.a)))(_ ++ _)

  private def findFunctionDeclarations(s: Stmt): List[LocalVariable] =
    s.getInnerStmt.map(findLocalVar).fold(s.getFunDecl.map(v => new LocalVariable(v.name.a)))(_ ++ _)
}

case class CompoundStmt(inner: List[Stmt]) extends Stmt {
  def toVM(isLocal: String => Boolean): Statement = inner.map(_.toVM(isLocal)).reverse.fold(emptyStatement)(_ ++ _)

  override def getInnerStmt: List[Stmt] = inner
}

case class VarStmt(vars: List[VarDef]) extends Stmt {

  private def toDefStmt(isLocal: String => Boolean, n: String, e: Expr): Statement = {
    val (s, v) = e.toVM(isLocal)
    assert(isLocal(n))
    Assignment(new LocalVariable(n), v).copyPosition(this) ++ s
  }

  def toVM(isLocal: String => Boolean): Statement = vars.filter(_.init.isDefined).map(x => toDefStmt(isLocal, x.name.a, x.init.get)).fold(emptyStatement)(_ ++ _)

  override def getVarDecl: List[VarDef] = vars
}

case class EmptyStmt() extends Stmt {
  def toVM(isLocal: String => Boolean): Statement = emptyStatement
}


case class NotImplStmt(inner: Any) extends Stmt {
  def toVM(isLocal: String => Boolean): Statement = ???
}

case class VarDef(name: Id, init: Option[Expr])


trait Expr extends AST {

  def toVM(isLocal: String => Boolean): (Statement, Variable)

  //  def eval(env: Env): Value
}

case class BinExpr(a: Expr, op: String, b: Expr) extends Expr {

  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    var (s1, v1) = a.toVM(isLocal)
    var (s2, v2) = b.toVM(isLocal)
    var r = freshVar
    (OpStatement(r, v1, v2).copyPosition(this) ++ s1 ++ s2, r)
  }

}


case class AssignExpr(a: Expr, op: String, b: Expr) extends Expr {

  def toVM(isLocal: String => Boolean): (Statement, Variable) =
    a match {
      case FieldAcc(targ, field) =>
        val (s1, v1) = targ.toVM(isLocal)
        val (s2, v2) = b.toVM(isLocal)
        //TODO look up evaluation order
        (Store(v1, field.a, v2).copyPosition(this) ++ s1 ++ s2, v2)
      case _ =>
        val (s1, v1) = a.toVM(isLocal)
        val (s2, v2) = b.toVM(isLocal)
        //TODO look up evaluation order
        (Assignment(v1, v2).copyPosition(this) ++ s1 ++ s2, v1)

    }


}

case class ITEExpr(i: Expr, t: Expr, e: Expr) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val (si, vi) = i.toVM(isLocal)
    val (st, vt) = t.toVM(isLocal)
    val (se, ve) = e.toVM(isLocal)
    val r = freshVar
    (ConditionalStatement(Assignment(r, ve).copyPosition(this) ++ se, Assignment(r, vt).copyPosition(this) ++ st).copyPosition(this) ++ si, r)
  }


}

case class PostExpr(a: Expr, s: String) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = a.toVM(isLocal)
}


case class UnaryExpr(a: String, e: Expr) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = if (Set("+", "++", "-", "--", "!", "typeof") contains a) e.toVM(isLocal) else ???

}

case class FieldAcc(a: Expr, field: Id) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val (s, v) = a.toVM(isLocal)
    val r = freshVar
    (Load(r, v, field.a).copyPosition(this) ++ s, r)
  }
}

object DynFieldAcc {
  val magicDynFieldAccess = "$_"
}

case class DynFieldAcc(a: Expr, field: Expr) extends Expr {

  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val (fieldvm, fieldv) = field.toVM(isLocal)
    val (s, v) = a.toVM(isLocal)
    val r = freshVar
    (Load(r, v, DynFieldAcc.magicDynFieldAccess).copyPosition(this) ++ s ++ fieldvm, r)
  }
}


case class FunCall(a: Expr, args: List[Expr]) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val (fs, fv) = a.toVM(isLocal)
    val argV = args.map(_.toVM(isLocal))
    val argStmts: Statement = argV.map(_._1).foldRight(emptyStatement)(_ ++ _)

    val r = freshVar
    (Call(r, fv, thisVar, argV.map(_._2)).copyPosition(this) ++ argStmts ++ fs, r)
  }
}

case class NewExpr(a: Expr, args: List[Expr]) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val (fs, fv) = a.toVM(isLocal)
    val argV = args.map(_.toVM(isLocal))
    val argStmts: Statement = argV.map(_._1).foldRight(emptyStatement)(_ ++ _)

    val r = freshVar
    (Constructor(r, fv, argV.map(_._2)).copyPosition(this) ++ argStmts ++ fs, r)
  }
}

abstract class Function(param: List[Id], body: FunctionBody) {
  protected def isLocalToFun(name: String) =
    body.localVars.exists(_.name == name) ||
      body.functionDeclarations.exists(_.name == name) ||
      param.exists(_.a == name)
}

case class FunDeclaration(name: Id, param: List[Id], body: FunctionBody) extends Function(param, body) with Stmt {
  override def toVM(isLocal: String => Boolean): Statement = {
    assert(isLocal(name.a))
    val variable = new LocalVariable(name.a)
    FunDecl(
      variable,
      param.map(x => new LocalVariable(x.a)),
      body.localVars ++ body.functionDeclarations,
      body.toVM(isLocalToFun)).copyPosition(this)
  }

  override def getFunDecl: List[FunDeclaration] = this :: Nil

}

case class FunExpr(name: Option[Id], param: List[Id], body: FunctionBody) extends Function(param, body) with Expr {

  def toVM(): FunDecl = toFunDecl()

  def toVM(isLocal: String => Boolean): (FunDecl, Variable) = {
    val fd = toFunDecl()
    (fd, fd.v)
  }

  def toFunDecl(): FunDecl = FunDecl(
    freshVar,
    param.map(x => new LocalVariable(x.a)),
    body.localVars ++ body.functionDeclarations,
    body.toVM(isLocalToFun)).copyPosition(this)

}


case class Lit(a: String) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val r = freshVar
    (ConstAssignment(r, a).copyPosition(this), r)
  }
}

case class Id(a: String) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) =
    (emptyStatement, if (isLocal(a))
      new LocalVariable(a)
    else new ExternalVariable(a))
}

case class ConstExpr(a: String) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val r = freshVar
    (PrimAssignment(r).copyPosition(this), r)
  }
}

case class ObjExpr(m: List[(String, Expr)]) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val r = freshVar
    val objectConstVar = new AnonymousVariable()
    val objectConstStmt = ConstAssignment(objectConstVar, "Object")
    var result: Statement = Constructor(r, objectConstVar, Nil).copyPosition(this) ++ objectConstStmt
    for (field <- m) {
      val (s, v) = field._2.toVM(isLocal)
      result = Store(r, field._1, v).copyPosition(this) ++ s ++ result
    }

    (result, r)
  }
}

case class ArrayExpr(m: List[Expr]) extends Expr {
  //arrays are modeled as objects with a single field __elements__ that holds all elements
  def toVM(isLocal: String => Boolean): (Statement, Variable) = {
    val r = freshVar
    val objectConstVar = new AnonymousVariable()
    val objectConstStmt = ConstAssignment(objectConstVar, "Array")
    var result: Statement = Constructor(r, objectConstVar, Nil).copyPosition(this) ++ objectConstStmt
    for (field <- m) {
      val (s, v) = field.toVM(isLocal)
      result = Store(r, "__elements__", v).copyPosition(this) ++ s ++ result
    }

    (result, r)
  }
}


case class NotImplExpr(a: Any) extends Expr {
  def toVM(isLocal: String => Boolean): (Statement, Variable) = ???
}