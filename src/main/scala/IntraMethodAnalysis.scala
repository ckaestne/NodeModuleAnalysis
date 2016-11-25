package edu.cmu.cs.nodesec

/**
  * Created by ckaestne on 11/24/16.
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

class Fun(val f: FunDecl, _name: String = NameHelper.genFunctionName) extends Obj(_name)

class UnknownValue(_name: String = "unknown-" + NameHelper.genObjectName) extends Obj(_name) {
  override def isUnknown: Boolean = true
}

case class MethodReturnValue(target: Set[Value], thisObj: Set[Value], args: List[Set[Value]]) extends UnknownValue {
  override def toString: String = "ret-" + super.toString
}

class UnknownLoadValue(val stmt: Option[Load]) extends UnknownValue


case class Env(
                store: Map[Variable, Set[Value]],
                members: Map[Obj, Map[String, Set[Value]]],
                calls: Map[Call, Set[MethodReturnValue]],
                scopeObj: Obj
              ) {


  def lookupOpt(name: Variable): Option[Set[Value]] = store.get(name)


  def lookup(v: Variable): (Set[Value], Env) = {
    val r = lookupOpt(v)
    if (r.isDefined) (r.get, this)
    else {
      assert(v.isInstanceOf[NamedVariable])
      val name = v.asInstanceOf[NamedVariable].name
      this.lookupFieldFromObj(scopeObj, name, () => new UnknownValue("$scope-" + name))
    }
  }

  //assignment killing previous assignments
  def store(name: Variable, value: Set[Value]): Env = this.copy(store = store + (name -> value))

  //    def kill(name: Variable): Env = this.copy(store = store - name)

  def storeField(v1: Variable, f: String, values: Set[Value]): Env = {
    //stores to Primitive types are ignored
    var (vals, env) = lookup(v1)
    vals foreach {
      case o: Obj =>
        env = env.storeField(o, f, values)
      case _ => assert(false, "store to primitive value not expected")
    }
    env
  }

  def storeField(o: Obj, f: String, values: Set[Value]): Env = {
    val omembers = members.getOrElse(o, Map())
    this.copy(members = members + (o -> (omembers + (f -> values))))
  }

  def lookupField(v: Variable, f: String, loadStmt: Option[Load]): (Set[Value], Env) = {
    //a little complicated, because we want to reflect every read as a change
    //to the environment, such that the next read will produce the same
    //unknown object

    //furthermore to avoid infinite loops on cyclic access path's, we
    //follow Whaley's strategy to create cycles in the heap

    var (values, env) = lookup(v)
    var result: Set[Value] = Set()
    values foreach {
      case o: Obj =>
        val (v, e) = env.lookupFieldFromObj(o, f, () => createTargetObj(o, loadStmt))
        env = e
        result ++= v
      case _ => assert(false, "TODO? load from primitive value not expected")
    }

    (result, env)
  }

  private def lookupFieldFromObj(o: Obj, f: String, createIfNotExists: () => Obj): (Set[Value], Env) = {
    val newEnv = if (!members.contains(o))
      this.copy(members = members + (o -> Map(f -> Set[Value](createIfNotExists()))))
    else this
    val omembers = newEnv.members(o)
    if (!omembers.contains(f)) {
      val newomembers = omembers + (f -> Set[Value](createIfNotExists()))
      (newomembers(f), newEnv.copy(members = newEnv.members + (o -> newomembers)))
    } else (omembers(f), newEnv)
  }


  private def createTargetObj(o: Obj, loadStmt: Option[Load]): Obj = {
    if (loadStmt.isDefined) {
      //search for objects that point to this object; if any of those are unknown objects
      //from the same load instruction, we have found a cycle; return that object instead

      var todo: List[Obj] = o :: Nil
      var done: Set[Obj] = Set()
      while (todo.nonEmpty) {
        var obj = todo.head
        obj match {
          case a: UnknownLoadValue if a.stmt == loadStmt => return obj
          case _ =>
        }
        todo = todo.tail
        done += obj
        val directBases = findDirectBases(o)
        todo = (directBases -- done).toList ++ todo
      }
    }
    return new UnknownLoadValue(loadStmt)
  }

  //find any object with a field pointing to o
  private def findDirectBases(o: Obj): Set[Obj] = members.filter(_._2.exists(_._2 contains o)).keySet


  def union(that: Env): Env = {
    assert(this.scopeObj == that.scopeObj)
    Env(relUnion(this.store, that.store, () => Set()),
      rel3Union(this.members, that.members, () => Set()),
      relUnion(this.calls, that.calls, () => Set()),
      this.scopeObj
    )
  }

  private def relUnion[A, B](a: Map[A, Set[B]], b: Map[A, Set[B]], emptySet: () => Set[B]): Map[A, Set[B]] = {
    ((a.keySet ++ b.keySet) map { k => k -> (a.getOrElse(k, emptySet()) ++ b.getOrElse(k, emptySet())) }).toMap
  }

  private def rel3Union[A, B, C](a: Map[A, Map[B, Set[C]]], b: Map[A, Map[B, Set[C]]], emptySet: () => Set[C]): Map[A, Map[B, Set[C]]] = {
    ((a.keySet ++ b.keySet) map { k => k -> relUnion(a.getOrElse(k, Map.empty), b.getOrElse(k, Map.empty), emptySet) }).toMap
  }


  def addCall(c: Call, retVal: MethodReturnValue): Env =
    this.copy(calls = calls + (c -> (calls.getOrElse(c, Set()) + retVal)))

}

object Env {
  def empty = Env(Map(), Map(), Map(), new Obj())
}

/**
  * this class performs a flow-sensitive analysis within the method
  * and produces a summary of field reads, field writes, and calls
  * as part of the environment.
  *
  * the environment of one method can be composed with others
  */
class IntraMethodAnalysis {

  import AnalysisHelper._


  private def freshObject = new Obj()

  private def freshUnknownObject(v: Variable) = v match {
    case NamedVariable(name) => new UnknownValue("unknown-" + name)
    case _ => new UnknownValue()
  }


  type Field = String


  val scopeObj: Obj = Param("$scope")

  def analyzeScript(p: Statement): Env = {
    analyze(AnalysisHelper.wrapScript(p))
  }

  def analyze(fun: FunDecl): Env = {
    val store: Map[Variable, Set[Value]] = Map[Variable, Set[Value]]() ++
      fun.args.map(a => (a, Set[Value](new Param(a.name)))) + (returnVariable -> Set(PrimitiveValue))
    var env = Env(store, Map(), Map(), scopeObj)
    analyze(env, fun.body)
  }

  private def analyze(env: Env, p: Statement): Env = p match {
    case Sequence(inner) =>
      inner.foldRight(env)((s, env) => analyze(env, s))
    case ConditionalStatement(alt1, alt2) =>
      val env1 = analyze(env, alt1)
      val env2 = analyze(env, alt2)
      env1 union env2
    case LoopStatement(inner) =>
      val newEnv = analyze(env, inner).union(env)
      if (newEnv == env)
        newEnv
      else analyze(newEnv, p) //iterate until fixpoint
    case stmt =>
      transfer(env, stmt)
  }

  private def transfer(env: Env, p: Statement): Env = p match {
    case Assignment(l, r) =>
      val (v, newEnv) = env.lookup(r)
      newEnv.store(l, v)
    case Return(l) =>
      //same as assignment to a special "$return" variable
      val (v, newEnv) = env.lookup(l)
      newEnv.store(returnVariable, v)
    case OpStatement(l, a, b) =>
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
      env.store(f.v, Set(new Fun(f)))
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


  private def lookupArgs(env: Env, args: List[Variable]): String =
    args.map(env.lookup).map(_._1.mkString("[", ",", "]")).mkString(", ")


  private def assert3(c: Boolean, msg: String) =
    if (!c) throw new Analysis3Exception(msg)


}


class MethodCompositionAnalysis {

  import AnalysisHelper._

  def collectFunDecls(s: Statement): Set[FunDecl] = s match {
    case f: FunDecl => collectFunDecls(f.body) + f
    case Sequence(inner) => inner.foldLeft(Set[FunDecl]())(_ ++ collectFunDecls(_))
    case LoopStatement(inner) => collectFunDecls(inner)
    case ConditionalStatement(a, b) => collectFunDecls(a) ++ collectFunDecls(b)
    case _ => Set()
  }


  def analyzeScript(p: Statement): Env = {
    val mainFun = AnalysisHelper.wrapScript(p)
    val funDecls = collectFunDecls(mainFun)

    val summaries = for (funDecl <- funDecls)
      yield (funDecl, new IntraMethodAnalysis().analyze(funDecl))

    val result = compose(summaries)

    println("callToRequire(A):-call(X,A), pt(A, " + Integer.toHexString(mainFun.hashCode()) + "#param-require).\n" +
      "callToRequire(A)?")

    result
  }


  /**
    * this method composes the method summaries produced by `IntraMethodAnalysis`
    *
    * context insensitive for now (context sensitivity through copying requires cycle detection
    * for calls among modules first)
    */
  def compose(methodSummaries: Set[(FunDecl, Env)]): Env = {
    var result = Env.empty

    //    var inclusionEdges: Set[(Obj, Obj)] = Set()

    println(
      """
        |%rules
        |pt(A,C) :- pt(A,B), pt(B,C).
        |pt(A,B) :- call(A, F), return(F, B).
        |pt(A,B) :- arg(F,Z,A), callarg(F,Z,B).
        |pt(A,B) :- member(X, F, A), member(X, F, B).
        |pt(A,B) :- member(X, F, A), member(Y, F, B), pt(X, Y).
        |%""".stripMargin)

    methodSummaries.map(a => summaryToDatalog(a._1, a._2) + "\n%").foreach(println)



    result
  }

  def summaryToDatalog(fun: FunDecl, env: Env): String = {
    val prefix = Integer.toHexString(fun.hashCode()) + "#"
    var result: List[String] = Nil

    result ::= s"fun($prefix)."
    for ((arg, idx) <- fun.args.zipWithIndex;
         obj <- env.lookup(arg)._1)
      result ::= s"arg($prefix, ${idx + 1}, $prefix$obj)."

    for (retvar <- Set(returnVariable); obj <- env.lookup(retvar)._1)
      result ::= s"return(${prefix}, $prefix$obj)."

    for ((obj, children) <- env.members;
         (field, fieldvalues) <- children;
         fieldvalue <- fieldvalues)
      result ::= s"member(${prefix}$obj, $field, $prefix$fieldvalue)."

    for ((call, retVals) <- env.calls;
         retVal <- retVals;
         target <- retVal.target) {
      val targetStr = target match {
        case f: Fun => Integer.toHexString(f.f.hashCode()) + "#"
        case _ => prefix + target //TODO fixme
      }
      result ::= s"call($prefix$retVal, $targetStr)."
      for ((argSet, idx) <- retVal.args.zipWithIndex; arg <- argSet)
        result ::= s"callarg($targetStr, ${idx + 1}, $prefix$arg)."
    }




    result.reverse.mkString("\n")
  }


}


class Analysis3Exception(msg: String) extends RuntimeException(msg)


object AnalysisHelper {
  def wrapScript(p: Statement) =
    new FunDecl(new AnonymousVariable(), List(
      NamedVariable("module"), NamedVariable("require"), NamedVariable("exports")
    ), p)

  val returnVariable = NamedVariable("$return")
}

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