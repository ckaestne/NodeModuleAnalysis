package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.parser._

import scala.util.parsing.input.Positional


case class CFG(entry: Statement, exit: Statement,
               nodes: Set[Statement], successors: Map[Statement, Set[Statement]]) {

  def printNode(n: Statement): String = {
    var s = n match {
      case Sequence(inner) => inner.mkString("\\n")
      case s: EmptyStatement => ""
      case s => s.toString
    }
    if (n == entry)
      s = "<<init>>\\n" + s
    if (n == exit)
      s = s + "\\n<<exit>>"
    s
  }

  def toDot(): String = {
    val b = new StringBuilder()
    b.append("digraph G {\n")

    for (n <- nodes)
      b.append("\t\"n" + n.hashCode() + "\" [ shape=\"rectangle\"; label=\"" + printNode(n) + "\" ]\n")

    for ((m, ss) <- successors; s <- ss)
      b.append("\t\"n" + m.hashCode() + "\" -> \"n" + s.hashCode() + "\"\n")

    b.append("}")
    b.toString()
  }
}

object CFG {
  def empty = CFG(EmptyStatement(), EmptyStatement(), Set(), Map())


}


object CFGBuilder {

  type CFGNode = Statement


  case class CFGBuildingEnv(
                             breakTarget: Option[Stmt],
                             continueTarget: Option[Stmt],
                             initStmt: Stmt,
                             endStmt: Stmt,
                             localVariableNames: Set[String],
                             exceptionHandler: Option[Stmt],
                             finalizers: Option[(Stmt, Stmt)]
                           ) {
    def setBreakTarget(target: Stmt): CFGBuildingEnv = this.copy(breakTarget = Some(target))

    def setContinueTarget(target: Stmt): CFGBuildingEnv = this.copy(continueTarget = Some(target))

    /**
      * two finalizers, the first for normal execution, the second for exceptional control
      * flow (catches the exception and rethrows it again)
      *
      * @param target
      * @return
      */
    def setFinalizers(target: Option[(Stmt, Stmt)]): CFGBuildingEnv = this.copy(finalizers = target)

    def setExceptionHandler(target: Option[Stmt]): CFGBuildingEnv = this.copy(exceptionHandler = target)
  }

  case class IntermediateCFG(nodes: Map[Stmt, CFGNode], successors: Set[(CFGNode, Stmt)]) {
    def addNode(stmt: Stmt, node: CFGNode, succ: Stmt): IntermediateCFG = addNode(stmt, node, Set(succ))

    def addNode(stmt: Stmt, node: CFGNode, succ: Set[Stmt]): IntermediateCFG =
      IntermediateCFG(this.nodes + (stmt -> node), successors ++ succ.map(s => (node, s)))

    def toCFG(initStmt: Stmt, endStmt: Stmt): CFG = {
      var rnodes: Set[CFGNode] = Set()
      var rsuccessors: Map[Statement, Set[Statement]] = Map()
      var initNode = nodes(initStmt)
      var endNode = nodes(endStmt)
      var mergedNodeLookup: Map[CFGNode, CFGNode] = Map()

      var todo: List[CFGNode] = initNode :: Nil
      while (todo.nonEmpty) {
        var node = todo.head
        todo = todo.tail
        if (!(rnodes contains node)) {
          var nodesucc = successors.filter(_._1 == node).map(_._2)
          assert(nodesucc.nonEmpty || node == endNode, "no successor for node found")

          //if a single successor, merge with next node
          var merged: Set[CFGNode] = Set(node)
          while (nodesucc.size == 1 && singlePredecessor(nodesucc.head)) {
            val succ = nodes(nodesucc.head)

            node = node ++ succ
            merged += succ
            nodesucc = successors.filter(_._1 == succ).map(_._2)
          }
          mergedNodeLookup ++= merged.map(_ -> node)
          val nodesuccnodes = nodesucc.map(nodes)

          rnodes += node
          rsuccessors += (node -> nodesuccnodes)

          todo ++= nodesuccnodes
        }
      }

      CFG(mergedNodeLookup(initNode), mergedNodeLookup(endNode),
        rnodes + mergedNodeLookup(initNode) + mergedNodeLookup(endNode),
        rsuccessors.mapValues(_.map(mergedNodeLookup)))
    }

    private def singlePredecessor(stmt: Stmt): Boolean =
      successors.filter(_._2 == stmt).size == 1

    def toDot(): String = {
      val b = new StringBuilder()
      b.append("digraph G {\n")

      //      nodes: Map[Stmt, CFGNode], successors: Set[(CFGNode, Stmt)]

      for ((s, n) <- nodes) {
        b.append("\t\"n" + n.hashCode() + "\" [ shape=\"rectangle\"; label=\"" + printNode(n) + "\" ]\n")
        b.append("\t\"n" + s.hashCode() + "\" [ label=\"" + s.toString + "\" ]\n")
        b.append("\t\"n" + s.hashCode() + "\" -> \"n" + n.hashCode() + "\"\n")
      }

      for ((n, s) <- successors)
        b.append("\t\"n" + n.hashCode() + "\" -> \"n" + s.hashCode() + "\"\n")

      b.append("}")
      b.toString()
    }

    private def printNode(n: Statement): String = n match {
      case Sequence(inner) => inner.mkString("\\n")
      case s: EmptyStatement => ""
      case s => s.toString
    }
  }

  def buildCFG(stmt: Stmt): CFG = {
    val startNode = stmt
    val endNode = EmptyStmt()
    val env = CFGBuildingEnv(None, None, startNode, endNode, Set(), None, None)
//    println(buildCFG(IntermediateCFG(
//      Map(endNode -> EmptyStatement()),
//      Set()), stmt, endNode, env).toDot())
    buildCFG(IntermediateCFG(
      Map(endNode -> EmptyStatement()),
      Set()), stmt, endNode, env).toCFG(startNode, endNode)
  }

  import VariableHelper._

  /**
    * builds intermediate structure for the AST
    *
    * @param stmt             statement to translate
    * @param defaultSuccessor default sucessor
    * @param env              targets for various nonlocal jumps
    * @return AST->Statement mapping and successor ASTs
    */
  private def buildCFG(cfg: IntermediateCFG, stmt: Stmt, defaultSuccessor: Stmt, env: CFGBuildingEnv): IntermediateCFG = {
    def _buildExpressionStmt(ast: Expr): (Statement, Variable) = buildExpressionStmt(ast, env)
    def _simpleStmt(node: CFGNode) = cfg.addNode(stmt, node.copyPosition(stmt), defaultSuccessor)
    stmt match {
      case IfStmt(expr, t, e) =>
        //t and e are successors; if there is no else branch, then defaultSucc is a successor
        val successors = Set() + t + e.getOrElse(defaultSuccessor)
        var ncfg = cfg.addNode(stmt, _buildExpressionStmt(expr)._1, successors)
        ncfg = buildCFG(ncfg, t, defaultSuccessor, env)
        if (e.isDefined)
          ncfg = buildCFG(ncfg, e.get, defaultSuccessor, env)
        ncfg
      case WhileStmt(expr, body) =>
        val ncfg = cfg.addNode(stmt, _buildExpressionStmt(expr)._1, Set(body, defaultSuccessor))
        buildCFG(ncfg, body, stmt, env.setBreakTarget(defaultSuccessor).setContinueTarget(stmt))
      case ExpressionStmt(expr) =>
        _simpleStmt(_buildExpressionStmt(expr)._1)
      case ForInStmt(_, left, right, body, each) =>
        ???
      case CompoundStmt(Nil) =>
        _simpleStmt(EmptyStatement())
      case CompoundStmt(inner) =>
        //empty statement followed by the first statement
        var ncfg = cfg.addNode(stmt, EmptyStatement(), inner.head)
        var innerList = inner
        while (innerList.nonEmpty) {
          //each statement followed by the next, except for last followed by default
          val s = innerList.head
          innerList = innerList.tail
          ncfg = buildCFG(ncfg, s, innerList.headOption.getOrElse(defaultSuccessor), env)
        }
        ncfg
      case TryStmt(block, handler, finalizer) =>
        val finalizers = finalizer.map(getExceptionFinalizer)
        var handlerStmt: Option[Stmt] = handler.map(h => CompoundStmt(List(
          ExpressionStmt(AssignExpr(h._1, "=", Id(AnalysisHelper.exceptionVariable.name))),
          h._2)))

        var ncfg = cfg.addNode(stmt, EmptyStatement(), block)
        ncfg = buildCFG(ncfg, block, finalizer.getOrElse(defaultSuccessor), env.setExceptionHandler(handlerStmt).setFinalizers(finalizers))
        if (handlerStmt.isDefined) {
          ncfg = buildCFG(ncfg, handlerStmt.get, finalizer.getOrElse(defaultSuccessor), env.setExceptionHandler(None).setFinalizers(finalizers))
        }
        if (finalizers.isDefined) {
          ncfg = buildCFG(ncfg, finalizers.get._1, defaultSuccessor, env)
          ncfg = buildCFG(ncfg, finalizers.get._2, defaultSuccessor, env)
        }
        ncfg


      case ReturnStmt(Some(expr)) =>
        val (s, v) = _buildExpressionStmt(expr)
        cfg.addNode(stmt, Assignment(AnalysisHelper.returnVariable, v).copyPosition(stmt) ++ s, env.endStmt)
      case ReturnStmt(None) =>
        val r = freshVar
        //if a finalizer exists jump to it (non-exception version) otherwise jump to end
        val jumpTarget = env.finalizers.map(_._1).getOrElse(env.endStmt)
        cfg.addNode(stmt, Assignment(AnalysisHelper.returnVariable, r).copyPosition(stmt) ++ PrimAssignment(r).copyPosition(stmt), jumpTarget)
      case ThrowStmt(expr) =>
        val (s, v) = _buildExpressionStmt(expr)
        //if a handler exists, jump there, else jump to the exception version of the finalizer
        val exceptionTarget = env.exceptionHandler.orElse(env.finalizers.map(_._2))
        if (exceptionTarget.isDefined)
          cfg.addNode(stmt, Assignment(AnalysisHelper.exceptionVariable, v).copyPosition(stmt) ++ s, exceptionTarget.get)
        // if neither exist, just model this as a return statement
        else
          cfg.addNode(stmt, Assignment(AnalysisHelper.returnVariable, v).copyPosition(stmt) ++ s, env.endStmt)
      case ContinueStmt(None) =>
        cfg.addNode(stmt, EmptyStatement(), env.continueTarget.get)
      case BreakStmt(None) =>
        cfg.addNode(stmt, EmptyStatement(), env.breakTarget.get)
      case VarStmt(vars) =>
        def toDefStmt(n: String, e: Expr): Statement = {
          val (s, v) = _buildExpressionStmt(e)
          Assignment(new LocalVariable(n), v).copyPosition(stmt) ++ s
        }
        _simpleStmt(vars.filter(_.init.isDefined).map(x => toDefStmt(x.name.a, x.init.get)).fold(EmptyStatement())(_ ++ _))
      case EmptyStmt() => _simpleStmt(EmptyStatement())
      case NotImplStmt(_) => ???

      case FunDeclaration(name, param, body) =>
        ???
      //        assert(env.localVariableNames.contains(name.a))
      //        val variable = new LocalVariable(name.a)
      //        FunDecl(
      //          variable,
      //          param.map(x => new LocalVariable(x.a)),
      //          body.localVars ++ body.functionDeclarations,
      //          body.toVM(isLocalToFun)).copyPosition(stmt)


    }

  }

  /**
    * for our analysis we do not need to catch the exception, just rethrow it
    * at the end. it's easy because it's still in the $exception variable
    * from the previous throw declarations. other throw declarations within
    * the final block will overwrite it correctly
    *
    * @param finalizer
    * @return
    */
  private def getExceptionFinalizer(finalizer: Stmt): (Stmt, Stmt) =
  (finalizer, CompoundStmt(List(finalizer.astCopy(), ThrowStmt(Id(AnalysisHelper.exceptionVariable.name)))))


  def buildExpressionStmt(expr: Expr, env: CFGBuildingEnv): (Statement, Variable) = {
    import VariableHelper._
    def _buildExpressionStmt(ast: Expr): (Statement, Variable) = buildExpressionStmt(ast, env)
    val (s, v) = expr match {
      case BinExpr(a, _, b) =>
        val (s1, v1) = _buildExpressionStmt(a)
        val (s2, v2) = _buildExpressionStmt(b)
        val r = freshVar
        (OpStatement(r, v1, v2) ++ s1 ++ s2, r)

      case AssignExpr(FieldAcc(targ, field), _, b) =>
        val (s1, v1) = _buildExpressionStmt(targ)
        val (s2, v2) = _buildExpressionStmt(b)
        //TODO look up evaluation order
        (Store(v1, field.a, v2) ++ s1 ++ s2, v2)

      case AssignExpr(a, _, b) =>
        val (s1, v1) = _buildExpressionStmt(a)
        val (s2, v2) = _buildExpressionStmt(b)
        //TODO look up evaluation order
        (Assignment(v1, v2) ++ s1 ++ s2, v1)

      case ITEExpr(i, t, e) =>
        val (si, vi) = _buildExpressionStmt(i)
        val (st, vt) = _buildExpressionStmt(t)
        val (se, ve) = _buildExpressionStmt(e)
        val r = freshVar
        (ConditionalStatement(Assignment(r, ve) ++ se, Assignment(r, vt) ++ st) ++ si, r)

      case PostExpr(e, _) => _buildExpressionStmt(e)
      case UnaryExpr(a, e) =>
        if (Set("+", "++", "-", "--", "!", "typeof") contains a)
          _buildExpressionStmt(e)
        else ???

      case FieldAcc(a, field) =>
        val (s, v) = _buildExpressionStmt(a)
        val r = freshVar
        (Load(r, v, field.a) ++ s, r)

      case DynFieldAcc(a, field) =>
        val (fieldvm, fieldv) = _buildExpressionStmt(field)
        val (s, v) = _buildExpressionStmt(a)
        val r = freshVar
        (Load(r, v, DynFieldAcc.magicDynFieldAccess) ++ s ++ fieldvm, r)

      case FunCall(a, args) =>
        val (fs, fv) = _buildExpressionStmt(a)
        val argV = args.map(_buildExpressionStmt)
        val argStmts: Statement = argV.map(_._1).foldRight(emptyStatement)(_ ++ _)

        val r = freshVar
        (Call(r, fv, thisVar, argV.map(_._2)) ++ argStmts ++ fs, r)

      case NewExpr(a, args) =>
        val (fs, fv) = _buildExpressionStmt(a)
        val argV = args.map(_buildExpressionStmt)
        val argStmts: Statement = argV.map(_._1).foldRight(emptyStatement)(_ ++ _)

        val r = freshVar
        (Constructor(r, fv, argV.map(_._2)) ++ argStmts ++ fs, r)

      case FunExpr(name, param, body) =>
        ???
      //       FunDecl( freshVar,
      //    param.map(x => new LocalVariable(x.a)),
      //    body.localVars ++ body.functionDeclarations,
      //    body.toVM(isLocalToFun)).copyPosition(this)

      case Lit(a) =>
        val r = freshVar
        (ConstAssignment(r, a), r)

      case Id(a) if env.localVariableNames contains a =>
        (emptyStatement, new LocalVariable(a))
      case Id(a) =>
        (emptyStatement, new ExternalVariable(a))

      case ConstExpr(a) =>
        val r = freshVar
        (PrimAssignment(r), r)

      case ObjExpr(m) =>
        val r = freshVar
        val objectConstVar = new AnonymousVariable()
        val objectConstStmt = ConstAssignment(objectConstVar, "Object")
        var result: Statement = Constructor(r, objectConstVar, Nil) ++ objectConstStmt
        for (field <- m) {
          val (s, v) = _buildExpressionStmt(field._2)
          result = Store(r, field._1, v) ++ s ++ result
        }
        (result, r)

      case ArrayExpr(m) =>
        val r = freshVar
        val objectConstVar = new AnonymousVariable()
        val objectConstStmt = ConstAssignment(objectConstVar, "Array")
        var result: Statement = Constructor(r, objectConstVar, Nil) ++ objectConstStmt
        for (field <- m) {
          val (s, v) = _buildExpressionStmt(field)
          result = Store(r, "__elements__", v) ++ s ++ result
        }

        (result, r)

      case NotImplExpr(_) => ???
    }
    (copyPosition(s, expr), v)
  }


  private def copyPosition(s: Statement, pos: Positional): Statement = s match {
    case Sequence(inner: Statement) => inner.foreach(_.copyPosition(pos)); s.copyPosition(pos)
    case s => s.copyPosition(pos)
  }

}


trait Statement extends Positional {
  def ++(s: Statement) = s match {
    case e: EmptyStatement => this
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
    case e: EmptyStatement => this
    case Sequence(i) => Sequence(s ++ i)
    case _ => Sequence(s :+ that)
  }

  override def ++(that: Sequence) = Sequence(this.s ++ that.s)

}

class EmptyStatement() extends Statement {
  override def ++(s: Statement) = s
}

object EmptyStatement {
  def apply() = new EmptyStatement()
}

case class Assignment(l: Variable, r: Variable) extends Statement

case class PrimAssignment(l: Variable) extends Statement

case class ConstAssignment(l: Variable, v: String) extends Statement

case class OpStatement(result: Variable, v1: Variable, v2: Variable) extends Statement

///**
//  * return is modeled as assignment to a special return variable
//  */
//object Return extends Statement {
//  def apply(l: Variable) = Assignment(l, AnalysisHelper.returnVariable)
//}
//
///**
//  * throw is modeled as assignment to a special variable
//  */
//object Throw extends Statement {
//  def apply(l: Variable) = Assignment(l, AnalysisHelper.exceptionVariable)
//}

case class Constructor(result: Variable, name: Variable, params: List[Variable]) extends Statement

case class Call(result: Variable, name: Variable, vthis: Variable, params: List[Variable]) extends Statement

case class Load(result: Variable, v: Variable, field: String) extends Statement

case class Store(target: Variable, field: String, v: Variable) extends Statement

case class FunDecl(v: Variable, args: List[LocalVariable], localVariables: List[LocalVariable], body: Statement) extends Statement {
  lazy val uniqueId = Integer.toHexString(hashCode()) + "#"
}

case class ConditionalStatement(alt1: Statement, alt2: Statement) extends Statement

case class LoopStatement(inner: Statement) extends Statement
