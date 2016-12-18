package edu.cmu.cs.nodesec.parser

import edu.cmu.cs.nodesec.analysis._

import scala.util.parsing.input.Positional

sealed trait AST extends Product with Positional {
  override def equals(that: Any) = that match {
    case thatRef: AnyRef => this eq thatRef
    case _ => false
  }
}


trait Stmt extends AST {
  def astCopy(): Stmt


  //  def toVM(isLocal: String => Boolean): Statement

  def getVarDecl: List[VarDef] = Nil

  def getInnerStmt: List[Stmt] = Nil

  def getFunDecl: List[FunDeclaration] = Nil

}

case class ExpressionStmt(expr: Expr) extends Stmt {
  //  def toVM(isLocal: String => Boolean): Statement = expr.toVM(isLocal)._1

  override def astCopy(): ExpressionStmt = ExpressionStmt(expr)
}

case class WhileStmt(expr: Expr, body: Stmt) extends Stmt {
  //  def toVM(isLocal: String => Boolean): Statement = LoopStatement(body.toVM(isLocal)).copyPosition(this) ++ expr.toVM(isLocal)._1

  override def getInnerStmt: List[Stmt] = List(body)

  override def astCopy(): WhileStmt = WhileStmt(expr, body.astCopy())
}

object DoWhileStmt {
  def apply(body: Stmt, expr: Expr) =
    CompoundStmt(List(body, WhileStmt(expr, body)))
}

case class IfStmt(expr: Expr, t: Stmt, e: Option[Stmt]) extends Stmt {
  //  def toVM(isLocal: String => Boolean): Statement =
  //    ConditionalStatement(e.map(s => s.toVM(isLocal)).getOrElse(emptyStatement), t.toVM(isLocal)).copyPosition(this) ++
  //      expr.toVM(isLocal)._1

  override def getInnerStmt: List[Stmt] = t :: e.map(List(_)).getOrElse(Nil)

  override def astCopy(): IfStmt = IfStmt(expr, t.astCopy(), e.map(_.astCopy()))
}

/**
  * vardecl does not contain an initializer; that is placed in init
  */
object ForStmt {
  def apply(varDecl: Option[VarDef], init: Option[Expr], test: Option[Expr], update: Option[Expr], body: Stmt): Stmt =
    CompoundStmt(List(
      varDecl.map(v => VarStmt(v :: Nil)).getOrElse(EmptyStmt()),
      init.map(ExpressionStmt).getOrElse(EmptyStmt()),
      WhileStmt(test.getOrElse(ConstExpr("true")),
        CompoundStmt(List(body, update.map(ExpressionStmt).getOrElse(EmptyStmt()))))
    ))
}


case class ForInStmt(varDecl: Option[VarDef], left: Expr, right: Expr, body: Stmt, each: Boolean) extends Stmt {
  //  def toVM(isLocal: String => Boolean): Statement = {
  //    val leftVm = left.toVM(isLocal)
  //    val rightVm = right.toVM(isLocal)
  //    val load = Load(leftVm._2, rightVm._2, DynFieldAcc.magicDynFieldAccess)
  //    LoopStatement(body.toVM(isLocal) ++ load).copyPosition(this) ++ leftVm._1 ++ rightVm._1 //TODO order may not be precise
  //  }

  override def getInnerStmt: List[Stmt] = List(body)

  override def getVarDecl: List[VarDef] = varDecl.map(List(_)).getOrElse(Nil)

  override def astCopy(): ForInStmt = ForInStmt(varDecl, left, right, body.astCopy(), each)
}

case class ContinueStmt(label: Option[Id]) extends Stmt {
  //  override def toVM(isLocal: (String) => Boolean): Statement = ???

  override def astCopy(): ContinueStmt = ContinueStmt(label)
}

case class BreakStmt(label: Option[Id]) extends Stmt {
  //  override def toVM(isLocal: (String) => Boolean): Statement = ???

  override def astCopy(): BreakStmt = BreakStmt(label)
}

case class ThrowStmt(expr: Expr) extends Stmt {
  //  override def toVM(isLocal: (String) => Boolean): Statement = ???

  override def astCopy(): ThrowStmt = ThrowStmt(expr)
}

case class TryStmt(block: Stmt, handler: Option[(Expr, Stmt)], finalizer: Option[Stmt]) extends Stmt {

  override def astCopy(): TryStmt = TryStmt(block.astCopy(), handler.map(e => (e._1, e._2.astCopy())), finalizer.map(_.astCopy()))
}

case class ReturnStmt(expr: Option[Expr]) extends Stmt {
  override def astCopy(): ReturnStmt = ReturnStmt(expr)
}

case class FunctionBody(inner: List[Stmt]) extends AST {
  //  def toVM(isLocal: String => Boolean): Statement =
  //    inner.map(_.toVM(isLocal)).reverse.fold(emptyStatement)(_ ++ _)

  lazy val localVars = inner.map(findLocalVar).fold(List[LocalVariable]())(_ ++ _)

  lazy val functionDeclarations = inner.map(findFunctionDeclarations).fold(List[LocalVariable]())(_ ++ _)


  private def findLocalVar(s: Stmt): List[LocalVariable] =
    s.getInnerStmt.map(findLocalVar).fold(s.getVarDecl.map(v => new LocalVariable(v.name.a)))(_ ++ _)

  private def findFunctionDeclarations(s: Stmt): List[LocalVariable] =
    s.getInnerStmt.map(findLocalVar).fold(s.getFunDecl.map(v => new LocalVariable(v.name.a)))(_ ++ _)
}

case class CompoundStmt(inner: List[Stmt]) extends Stmt {

  override def getInnerStmt: List[Stmt] = inner

  override def astCopy(): CompoundStmt = CompoundStmt(inner.map(_.astCopy()))
}

case class VarStmt(vars: List[VarDef]) extends Stmt {


  override def getVarDecl: List[VarDef] = vars

  override def astCopy(): VarStmt = VarStmt(vars)
}

case class EmptyStmt() extends Stmt {
  override def astCopy(): EmptyStmt = EmptyStmt()
}


case class VarDef(name: Id, init: Option[Expr])


trait Expr extends AST {

  //  def eval(env: Env): Value
}

case class BinExpr(a: Expr, op: String, b: Expr) extends Expr


case class AssignExpr(a: Expr, op: String, b: Expr) extends Expr

case class ITEExpr(i: Expr, t: Expr, e: Expr) extends Expr

case class PostExpr(a: Expr, s: String) extends Expr

case class UnaryExpr(a: String, e: Expr) extends Expr

case class FieldAcc(a: Expr, field: Id) extends Expr

object DynFieldAcc {
  val magicDynFieldAccess = "$_"
}

case class DynFieldAcc(a: Expr, field: Expr) extends Expr


case class FunCall(a: Expr, args: List[Expr]) extends Expr

case class NewExpr(a: Expr, args: List[Expr]) extends Expr

abstract class Function(val param: List[Id], val body: FunctionBody) {
  protected def isLocalToFun(name: String) =
    body.localVars.exists(_.name == name) ||
      body.functionDeclarations.exists(_.name == name) ||
      param.exists(_.a == name)
}

case class FunDeclaration(name: Id, _param: List[Id], _body: FunctionBody) extends Function(_param, _body) with Stmt {
  override def getFunDecl: List[FunDeclaration] = this :: Nil

  override def astCopy(): Stmt = FunDeclaration(name, param, body)
}

case class FunExpr(name: Option[Id], _param: List[Id], _body: FunctionBody) extends Function(_param, _body) with Expr

//{
//
////  def toVM(): FunDecl = toFunDecl()
//
////  def toVM(isLocal: String => Boolean): (FunDecl, Variable) = {
////    val fd = toFunDecl()
////    (fd, fd.v)
////  }
//
////  def toFunDecl(): FunDecl = FunDecl(
////    freshVar,
////    param.map(x => new LocalVariable(x.a)),
////    body.localVars ++ body.functionDeclarations,
////    body.toVM(isLocalToFun)).copyPosition(this)
//
//}


case class Lit(a: String) extends Expr

case class Id(a: String) extends Expr

case class ConstExpr(a: String) extends Expr

case class ObjExpr(m: List[(String, Expr)]) extends Expr

case class ArrayExpr(m: List[Expr]) extends Expr


case class NotImplExpr(a: Any) extends Expr