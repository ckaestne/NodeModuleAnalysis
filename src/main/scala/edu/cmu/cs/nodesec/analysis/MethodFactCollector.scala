package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.datalog._

/**
  * Flow-insensitive fact collector (i.e. weak updates).
  * Directly collects relationships between variables and
  * values
  *
  * Created by ckaestne on 12/18/16.
  */
object MethodFactCollector {

  /**
    * collect facts of a single function (no mechanisms for composition yet)
    */
  def collectFunFacts(fun: Fun): Set[DFact] = {
    val instructionFacts: Set[Iterable[DFact]] = for (block <- fun.body.nodes;
                                                      instr <- block.s) yield instr match {
      case Assignment(l, r) =>
        Set(DAssign(fun, l, r))
      case PrimAssignment(_) => Nil
      case ConstAssignment(_, _) => Nil
      case OpInstruction(r, v1, op, v2) if Set("||", "&&", "ite") contains op =>
        Set(DAssign(fun, r, v1), DAssign(fun, r, v2))
      case OpInstruction(r, v1, op, v2) =>
        //TODO convert to string etc
        Nil
      case Constructor(r, name, params) =>
        val obj = new Obj()
        Set(DStack(fun, r, obj))
      case Call(r, name, vthis, params) =>
        var actuals = for ((param, idx) <- params.zipWithIndex) yield
          DActual(fun, name, idx, param)
        actuals :+ DInvoke(fun, name, r)
      case Load(r, v, f) =>
        val newObj = new Obj("toObj-" + NameHelper.genObjectName)
        Set(DLoad(fun, r, v, f), DStack(fun, v, newObj))
      case Store(r, f, v) =>
        val newObj = new Obj("toObj-" + NameHelper.genObjectName)
        Set(DStore(fun, r, f, v), DStack(fun, r, newObj))
      case FunDecl(r, f) =>
        val fobj = new Obj("fun-" + NameHelper.genObjectName)
        Set(DFunctionDecl(fun, fobj, f), DStack(fun, r, fobj))
    }
    var declFacts =
      (for ((formal, idx) <- fun.args.zipWithIndex) yield DFormal(fun, idx, formal)) :+
        DReturn(fun, VariableHelper.returnVariable)

    instructionFacts.flatten ++ declFacts.toSet
  }

  /**
    * collect facts of a function, including all inner functions
    *
    * returns a mapping to function only because it allows to print
    * facts separately for debugging purposes
    */
  def collectFacts(fun: Fun): Seq[(Fun, Set[DFact])] =
  composeWithInnerFunctions(fun)._2


  /**
    * hierarchical composition,
    *
    * returns datalog facts and updated set of closure variables
    */
  private[analysis] def composeWithInnerFunctions(fun: Fun): (Set[ExternalVariable], Seq[(Fun, Set[DFact])]) = {
    //start composition from child to parent
    val innerFunctions = fun.innerFunctions.map(f => (f, composeWithInnerFunctions(f)))
    val innerFacts = innerFunctions.toSeq.flatMap(_._2._2)

    var facts = collectFunFacts(fun)

    //connect closure variables
    val innerClosureVariables = innerFunctions.flatMap(_._2._1)
    val closureToLocal = innerClosureVariables.map(cl => cl -> fun.localOrArgs.find(_.name == cl.name)).filter(_._2.nonEmpty).toMap
    val propagatedClosure = innerClosureVariables -- closureToLocal.keys
    val outerClosure = fun.closureVariables ++ propagatedClosure
    for ((innerFun, (closureVars, _)) <- innerFunctions; closureVar <- closureVars) {
      if (closureToLocal contains closureVar)
        facts += DClosureToLocal(fun, closureToLocal(closureVar).get, innerFun, closureVar)
      if (propagatedClosure contains closureVar)
        facts += DClosureToClosure(fun, innerFun, closureVar)
    }

    (outerClosure, innerFacts :+ (fun, facts))
  }

}
