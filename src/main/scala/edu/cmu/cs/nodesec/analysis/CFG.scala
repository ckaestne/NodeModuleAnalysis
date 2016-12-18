package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.parser._

import scala.util.parsing.input.Positional


case class CFG(entry: Block, exit: Block,
               nodes: Set[Block], successors: Map[Block, Set[Block]]) {

  def printNode(n: Block): String = {
    var s = n.s.reverse.mkString("\\n")
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
  def empty = CFG(Block.empty(), Block.empty(), Set(), Map())


}


object CFGBuilder {

  type CFGNode = Block


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
      var rsuccessors: Map[CFGNode, Set[CFGNode]] = Map()
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

            node = succ ++ node
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

    private def printNode(n: CFGNode): String = n.s.reverse.mkString("\\n")
  }


  def findInnerFunctions(cfg: CFG): Set[Fun] =
    (for (node <- cfg.nodes; stmt <- node.s) yield stmt match {
      case FunDecl(_, f) => Set(f)
      case _ => Set[Fun]()
    }).flatten

  def findAllVars(cfg: CFG): Set[Variable] = (for (node <- cfg.nodes; stmt <- node.s) yield stmt match {
    case Assignment(l, r) => Set(l, r)
    case PrimAssignment(l) => Set(l)
    case ConstAssignment(l, _) => Set(l)
    case OpInstruction(a, b, _, c) => Set(a, b, c)
    case Constructor(a, b, cs) => cs.toSet + a + b
    case Call(a, b, c, ds) => ds.toSet + a + b + c
    case Load(a, b, _) => Set(a, b)
    case Store(a, _, b) => Set(a, b)
    case FunDecl(a, _) => Set(a)
  }).flatten

  def findNonlocalVars(cfg: CFG): Set[ExternalVariable] =
    findAllVars(cfg: CFG).flatMap({
      case e: ExternalVariable => Set(e)
      case _ => Set[ExternalVariable]()
    })

  //    (for (node <- cfg.nodes) yield node match {
  //      case
  //    }).flatten

  def toFun(functionAST: edu.cmu.cs.nodesec.parser.Function): Fun = {
    val localVars =
      functionAST.body.localVars.map(_.name) ++ functionAST.body.functionDeclarations.map(_.name) ++ functionAST.param.map(_.a)

    val startNode = CompoundStmt(functionAST.body.inner)
    val endNode = EmptyStmt()
    val env = CFGBuildingEnv(None, None, startNode, endNode, localVars.toSet, None, None)

    val cfg = buildCFG(IntermediateCFG(
      Map(endNode -> Block.empty()),
      Set()), startNode, endNode, env).toCFG(startNode, endNode)

    val innerFuns = findInnerFunctions(cfg)
    val nonlocalVars = findNonlocalVars(cfg)

    Fun(
      cfg,
      functionAST.param.map(p => LocalVariable(p.a)),
      (functionAST.body.localVars ++ functionAST.body.functionDeclarations).map(p => LocalVariable(p.name)).toSet,
      nonlocalVars,
      innerFuns
    )
  }


  def buildCFG(stmt: Stmt): CFG = {
    val startNode = stmt
    val endNode = EmptyStmt()
    val env = CFGBuildingEnv(None, None, startNode, endNode, Set(), None, None)
    //    println(buildCFG(IntermediateCFG(
    //      Map(endNode -> EmptyStatement()),
    //      Set()), stmt, endNode, env).toDot())
    buildCFG(IntermediateCFG(
      Map(endNode -> Block.empty()),
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
    def _buildExpressionStmt(ast: Expr): (CFGNode, Variable) = buildExpressionStmt(ast, env)
    def _simpleStmt(node: CFGNode) = cfg.addNode(stmt, copyPosition(node, stmt), defaultSuccessor)
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
        _simpleStmt(Block.empty())
      case CompoundStmt(inner) =>
        //empty statement followed by the first statement
        var ncfg = cfg.addNode(stmt, Block.empty(), inner.head)
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
          ExpressionStmt(AssignExpr(h._1, "=", Id(VariableHelper.exceptionVariable.name))),
          h._2)))

        var ncfg = cfg.addNode(stmt, Block.empty(), block)
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
        cfg.addNode(stmt, Assignment(VariableHelper.returnVariable, v).copyPosition(stmt) ++ s, env.endStmt)
      case ReturnStmt(None) =>
        val r = freshVar
        //if a finalizer exists jump to it (non-exception version) otherwise jump to end
        val jumpTarget = env.finalizers.map(_._1).getOrElse(env.endStmt)
        cfg.addNode(stmt, Assignment(VariableHelper.returnVariable, r).copyPosition(stmt) ++ PrimAssignment(r).copyPosition(stmt), jumpTarget)
      case ThrowStmt(expr) =>
        val (s, v) = _buildExpressionStmt(expr)
        //if a handler exists, jump there, else jump to the exception version of the finalizer
        val exceptionTarget = env.exceptionHandler.orElse(env.finalizers.map(_._2))
        if (exceptionTarget.isDefined)
          cfg.addNode(stmt, Assignment(VariableHelper.exceptionVariable, v).copyPosition(stmt) ++ s, exceptionTarget.get)
        // if neither exist, just model this as a return statement
        else
          cfg.addNode(stmt, Assignment(VariableHelper.returnVariable, v).copyPosition(stmt) ++ s, env.endStmt)
      case ContinueStmt(None) =>
        cfg.addNode(stmt, Block.empty(), env.continueTarget.get)
      case BreakStmt(None) =>
        cfg.addNode(stmt, Block.empty(), env.breakTarget.get)
      case VarStmt(vars) =>
        def toDefStmt(n: String, e: Expr): CFGNode = {
          val (s, v) = _buildExpressionStmt(e)
          Assignment(new LocalVariable(n), v).copyPosition(stmt) ++ s
        }
        _simpleStmt(vars.filter(_.init.isDefined).map(x => toDefStmt(x.name.a, x.init.get)).fold(Block.empty())(_ ++ _))
      case EmptyStmt() => _simpleStmt(Block.empty())
      case NotImplStmt(_) => ???

      case f@FunDeclaration(name, param, body) =>
        assert(env.localVariableNames.contains(name.a))
        val variable = new LocalVariable(name.a)
        _simpleStmt(FunDecl(variable, toFun(f)).copyPosition(stmt).toBlock)

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
  (finalizer, CompoundStmt(List(finalizer.astCopy(), ThrowStmt(Id(VariableHelper.exceptionVariable.name)))))


  def buildExpressionStmt(expr: Expr, env: CFGBuildingEnv): (CFGNode, Variable) = {
    import VariableHelper._
    def _buildExpressionStmt(ast: Expr): (CFGNode, Variable) = buildExpressionStmt(ast, env)
    val (s, v): (CFGNode, Variable) = expr match {
      case BinExpr(a, op, b) =>
        val (s1, v1) = _buildExpressionStmt(a)
        val (s2, v2) = _buildExpressionStmt(b)
        val r = freshVar
        (OpInstruction(r, v1, op, v2) ++ s1 ++ s2, r)

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
        //TODO technically this could be modeled as control flow branch
        (OpInstruction(r, ve, "ite", vt) ++ se ++ st ++ si, r)

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
        val argStmts: CFGNode = argV.map(_._1).foldRight(Block.empty())(_ ++ _)

        val r = freshVar
        (Call(r, fv, thisVar, argV.map(_._2)) ++ argStmts ++ fs, r)

      case NewExpr(a, args) =>
        val (fs, fv) = _buildExpressionStmt(a)
        val argV = args.map(_buildExpressionStmt)
        val argStmts: CFGNode = argV.map(_._1).foldRight(Block.empty())(_ ++ _)

        val r = freshVar
        (Constructor(r, fv, argV.map(_._2)) ++ argStmts ++ fs, r)

      case f@FunExpr(name, param, body) =>
        val v = freshVar
        (FunDecl(v, toFun(f)).copyPosition(expr).toBlock, v)

      case Lit(a) =>
        val r = freshVar
        (ConstAssignment(r, a).toBlock, r)

      case Id(a) if env.localVariableNames contains a =>
        (Block.empty(), new LocalVariable(a))
      case Id(a) =>
        (Block.empty(), new ExternalVariable(a))

      case ConstExpr(a) =>
        val r = freshVar
        (PrimAssignment(r).toBlock, r)

      case ObjExpr(m) =>
        val r = freshVar
        val objectConstVar = new AnonymousVariable()
        val objectConstStmt = ConstAssignment(objectConstVar, "Object")
        var result: CFGNode = Constructor(r, objectConstVar, Nil) ++ objectConstStmt
        for (field <- m) {
          val (s, v) = _buildExpressionStmt(field._2)
          result = Store(r, field._1, v) ++ s ++ result
        }
        (result, r)

      case ArrayExpr(m) =>
        val r = freshVar
        val objectConstVar = new AnonymousVariable()
        val objectConstStmt = ConstAssignment(objectConstVar, "Array")
        var result: CFGNode = Constructor(r, objectConstVar, Nil) ++ objectConstStmt
        for (field <- m) {
          val (s, v) = _buildExpressionStmt(field)
          result = Store(r, "__elements__", v) ++ s ++ result
        }

        (result, r)

      case NotImplExpr(_) => ???
    }
    (copyPosition(s, expr), v)
  }


  private def copyPosition(s: CFGNode, pos: Positional): CFGNode = {
    s.s.foreach(_.copyPosition(pos));
    s
  }

  private def copyPosition(s: Instruction, pos: Positional): Instruction = {
    s.copyPosition(pos);
    s
  }

}


trait Instruction extends Positional {
  def ++(s: Instruction) = toBlock ++ s

  def ++(s: Block) = Block(this :: s.s)

  def toBlock = Block(List(this))

  override def equals(o: scala.Any): Boolean = o match {
    case that: Instruction => this eq that
    case _ => false
  }

  def copyPosition(p: Positional): this.type = {
    this.setPos(p.pos)
    this
  }


}

/**
  * a block contains a list of instructions; the
  * first instruction is at the tail of the list and later
  * instructions are added to the head
  *
  * @param s
  */
case class Block(s: List[Instruction]) {
  def ++(that: Instruction) = Block(s :+ that)


  def ++(that: Block) = Block(s ++ that.s)

  override def equals(o: scala.Any): Boolean = o match {
    case that: Block => this eq that
    case _ => false
  }
}

object Block {
  def empty(): Block = Block(Nil)
}


case class Assignment(l: Variable, r: Variable) extends Instruction

case class PrimAssignment(l: Variable) extends Instruction

case class ConstAssignment(l: Variable, v: String) extends Instruction

case class OpInstruction(result: Variable, v1: Variable, op: String, v2: Variable) extends Instruction

case class Constructor(result: Variable, name: Variable, params: List[Variable]) extends Instruction

case class Call(result: Variable, name: Variable, vthis: Variable, params: List[Variable]) extends Instruction

case class Load(result: Variable, v: Variable, field: String) extends Instruction

case class Store(target: Variable, field: String, v: Variable) extends Instruction

case class FunDecl(v: Variable, fun: Fun) extends Instruction {
  override def toString: String = s"FunDecl($v, ${fun.uniqueId})"
}


/**
  * a function is a unit of analysis
  * functions form a tree structure depending on their nesting when defined
  */
case class Fun(body: CFG,
               args: List[LocalVariable], localVariables: Set[LocalVariable], closureVariables: Set[ExternalVariable],
               innerFunctions: Set[Fun]) {
  lazy val uniqueId = Integer.toHexString(hashCode()) + "#"
  lazy val localOrArgs = args ++ localVariables

  //direct and indirect inner functions
  lazy val allInnerFunctions: Set[Fun] = innerFunctions ++ innerFunctions.flatMap(_.allInnerFunctions)
  //closure variables including those from inner functions that are not matched by local variables
  lazy val allClosureVariables: Set[ExternalVariable] = {
    val innerClosureVariables = innerFunctions.flatMap(_.allClosureVariables)
    val closureToLocal = innerClosureVariables.map(cl => cl -> localOrArgs.find(_.name == cl.name)).filter(_._2.nonEmpty).toMap
    val propagatedClosure = innerClosureVariables -- closureToLocal.keys
    closureVariables ++ propagatedClosure
  }

}