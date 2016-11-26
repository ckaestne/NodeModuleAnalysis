package edu.cmu.cs.nodesec.analysis

/**
  * Created by ckaestne on 11/25/16.
  */


case class Env(
                store: Map[Variable, Set[Value]],
                members: Map[Obj, Map[String, Set[Value]]],
                calls: Map[Call, Set[MethodReturnValue]],
                functionPtrs: Set[(Obj, FunDecl)],
                localScopeObj: Obj,
                closureObj: Obj
              ) {


  def lookupOpt(name: Variable): Option[Set[Value]] = store.get(name)


  def lookup(v: Variable): (Set[Value], Env) = v match {
    //named variables are actually fields of the scope
    case LocalVariable(n) =>
      this.lookupFieldFromObj(localScopeObj, n, () => new UnknownValue("$scope-" + n))
    case ExternalVariable(n) =>
      this.lookupFieldFromObj(closureObj, n, () => new UnknownValue("$scope-" + n))
    case _: AnonymousVariable =>
      (store(v), this)
  }

  //assignment killing previous assignments
  def store(name: Variable, value: Set[Value]): Env = name match {
    //named variables are actually fields of the scope
    case LocalVariable(n) =>
      storeField(localScopeObj, n, value)
    case ExternalVariable(n) =>
      storeField(closureObj, n, value)
    case _: AnonymousVariable => this.copy(store = store + (name -> value))
  }

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
    assert(this.localScopeObj == that.localScopeObj)
    assert(this.closureObj == that.closureObj)
    Env(relUnion(this.store, that.store, () => Set()),
      rel3Union(this.members, that.members, () => Set()),
      relUnion(this.calls, that.calls, () => Set()),
      this.functionPtrs ++ that.functionPtrs,
      this.localScopeObj,
      this.closureObj
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

  def addFunctionPtr(funObj: Fun, fun: FunDecl): Env = copy(functionPtrs = functionPtrs.+((funObj, fun)))

}

object Env {
  def empty = Env(Map(), Map(), Map(), Set(), new Obj(), new Obj())
}