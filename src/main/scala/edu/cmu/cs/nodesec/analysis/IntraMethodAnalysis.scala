package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.parser.FunctionBody

/**
  * Created by ckaestne on 11/24/16.
  */


/**
  * this class performs a flow-sensitive analysis within the method
  * and produces a summary of field reads, field writes, and calls
  * as part of the environment.
  *
  * the environment of one method can be composed with others
  */
class IntraMethodAnalysis {

  import VariableHelper._


  private def freshObject = new Obj()

  private def freshUnknownObject(v: Variable) = v match {
    case LocalVariable(name) => new UnknownValue("unknown-" + name)
    case _ => new UnknownValue()
  }


  type Field = String


  def analyzeScript(fun: FunctionBody): MethodSummary =
    analyze(AnalysisHelper.cfgScript(fun))

  def analyze(fun: Fun): MethodSummary = {
    //initialize store with parameters and return value; assign all parameters and local variables as members of the scope
    val closureScopeObj: Obj = Param("$closure")
    val localScopeObj: Obj = Param("$local")
    val params = fun.args.map(a => (a, Set[Value](new Param(a.name))))
    val locals = fun.localVariables.map(a => (a, Set[Value](PrimitiveValue))) +
      ((LocalVariable("global"), Set[Value](closureScopeObj))) //`global` is a local variable pointing to the global scope
    val store: Map[Variable, Set[Value]] = Map[Variable, Set[Value]]() ++
      (params ++ locals).toMap + (returnVariable -> Set(PrimitiveValue))
    val members: Map[Obj, Map[String, Set[Value]]] =
      Map(localScopeObj -> (params ++ locals).map(a => (a._1.name, a._2)).toMap)
    val env = MethodSummary.empty.copy(store = store, members = members, localScopeObj = localScopeObj, closureObj = closureScopeObj)
    analyzeCFG(env, fun.body)
  }

  //  private def computePredecessors(successors: Map[Block, Set[Block]]): Map[Block, Set[Block]] = {
  //    var result = Map[Block, Set[Block]]()
  //    for ((a, bs) <- successors; b <- bs) {
  //      result += (b -> (result.getOrElse(b, Set()) + a))
  //    }
  //    result
  //  }

  private def analyzeCFG(initialEnv: MethodSummary, cfg: CFG): MethodSummary = {
    //simple worklist algorithm
    var inputEnvs: Map[Block, MethodSummary] = Map(cfg.entry -> initialEnv)
    var worklist: Seq[Block] = bfs(cfg)
    while (worklist.nonEmpty) {
      val block = worklist.head
      worklist = worklist.tail

      val sourceEnv = inputEnvs(block)
      val resultingEnv = transfer(sourceEnv, block)
      val successors = cfg.successors(block)
      //if env has changed, put all successors on end of worklist that are not already on there
      for (succ <- successors
           if !(inputEnvs contains succ) || inputEnvs(succ) != resultingEnv) {
        if (!(inputEnvs contains succ))
          inputEnvs += (succ -> resultingEnv)
        else
          inputEnvs += (succ -> inputEnvs(succ).union(resultingEnv))
        if (!(worklist contains succ))
          worklist = worklist :+ succ
      }
    }
    transfer(inputEnvs(cfg.exit), cfg.exit)
  }

  private def bfs(cfg: CFG): Seq[Block] = {
    var result = Seq[Block]()
    var worklist = cfg.entry :: Nil
    while (worklist.nonEmpty) {
      val block = worklist.head
      worklist = worklist.tail

      result = result :+ block

      worklist = worklist ++ (cfg.successors.getOrElse(block, Set()) -- worklist -- result)
    }
    result
  }

  //apply to every statement, from tail to head (due to reverse
  //instruction order in block.s)
  private def transfer(env: MethodSummary, p: Block): MethodSummary =
  p.s.foldRight(env)((s, env) => transfer(env, s))

  private def transfer(env: MethodSummary, p: Instruction): MethodSummary = p match {
    case Assignment(l, r) =>
      val (v, newEnv) = env.lookup(r)
      newEnv.store(l, v)
    //    case Return(l) =>
    //      //same as assignment to a special "$return" variable
    //      val (v, newEnv) = env.lookup(l)
    //      newEnv.store(returnVariable, v)
    case OpInstruction(l, a, b) =>
      val (va, newEnv) = env.lookup(a)
      val (vb, newEnv2) = newEnv.lookup(a)
      newEnv2.store(l, va ++ vb)
    case PrimAssignment(l) =>
      env.store(l, Set(PrimitiveValue))
    case ConstAssignment(v, s) =>
      env.store(v, Set(new Constant(s)))
    case c@Call(v, v0, vthis, vargs) =>
      val (receiver, env1) = env.lookup(v0)
      val (othis, env2) = env1.lookup(vthis)
      var enva = env2
      val oargs = for (varg <- vargs) yield {
        val (oarg, _env) = enva.lookup(varg)
        enva = _env
        oarg
      }
      //      assert3(!(receiver contains requireObject), "call to require function found with arguments " + lookupArgs(env, vargs) + " -- " + receiver)
      //      assert3(!receiver.exists(_.isUnknown), "call to unknown value found")
      val retVal = new MethodReturnValue(receiver, othis, oargs)
      enva.store(v, Set(retVal)).addCall(c, retVal)
    case f: FunDecl =>
      val funObj = new FunctionValue(f.fun)
      env.store(f.v, Set(funObj)).addFunctionPtr(funObj, f.fun)
    case Store(v1, f, v2) =>
      val (receiver, newEnv) = env.lookup(v1)
      //      receiver.foreach(v => assert3(!v.isUnknown, s"store to field unknown object found ($v.$f)"))
      val (value, newEnv2) = newEnv.lookup(v2)
      newEnv2.storeField(v1, f, value)
    case p@Load(v1, v2, f) =>
      val (value, newEnv) = env.lookupField(v2, f, Some(p))
      newEnv.store(v1, value)
    case Constructor(v, clsNameVar, params) =>
      //TODO prototype
      env.store(v, Set(freshObject))
  }


  private def lookupArgs(env: MethodSummary, args: List[Variable]): String =
    args.map(env.lookup).map(_._1.mkString("[", ",", "]")).mkString(", ")


  private def assert3(c: Boolean, msg: String) =
    if (!c) throw new Analysis3Exception(msg)


}


class Analysis3Exception(msg: String) extends RuntimeException(msg)


object NameHelper {
  var objectCounter = 0

  def genObjectName = {
    objectCounter += 1
    "obj" + objectCounter
  }

  var functionCounter = 0

  def genFunctionName = {
    functionCounter += 1
    "fun" + functionCounter
  }
}