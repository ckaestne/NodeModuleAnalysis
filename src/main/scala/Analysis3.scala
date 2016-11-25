package edu.cmu.cs.nodesec

/**
  * Created by ckaestne on 11/24/16.
  */
class Analysis3 {

  sealed trait Value

  case class Constant(s: String) extends Value

  class Obj(members: Map[String, Value]) extends Value
  class Fun(f: FunDecl) extends Value

  object PrimitiveValue extends Value

  object UnknownValue extends Value

  private def freshObject = new Obj(Map())

  val requireObject = freshObject

  trait Env {
    def lookup(name: Variable): Set[Value]

    def store(name: Variable, value: Set[Value])

    def put(name: Variable, value: Value) = store(name, lookup(name) + value)
  }

  case class Environment(var store: Map[Variable, Set[Value]], outer: Env) extends Env {
    override def lookup(name: Variable): Set[Value] = store.getOrElse(name, Set(UnknownValue))

    override def store(name: Variable, value: Set[Value]): Unit = store += (name -> value) //TODO merge with existing value
  }

  object EmptyEnvironment extends Env {
    override def lookup(name: Variable): Set[Value] = Set(UnknownValue)

    override def store(name: Variable, value: Set[Value]): Unit = ???
  }

  case class Context(env: Env, thisObj: Value) {
    def split(): Context =
      new Context(env)


  }

  def analyze(p: Statement): Unit = {
    val globalEnv = Environment(Map(), EmptyEnvironment)
    globalEnv.store(new NamedVariable("require"), Set(requireObject))
    val thisObj = freshObject
    a(Context(globalEnv, thisObj), p)
  }


  private def a(c: Context, p: Statement): Unit = p match {
    case Sequence(s) => s.reverse.foreach(a(c, _))
    case Assignment(l, r) =>
      c.env.store(l,
        c.env.lookup(r))
    case PrimAssignment(l) =>
      c.env.store(l, Set(PrimitiveValue))
    case ConstAssignment(v, s) =>
      c.env.store(v, Set(new Constant(s)))
    case Call(v, v0, vthis, vargs) =>
      val receiver = c.env.lookup(v0)
      assert3(!(receiver contains requireObject), "call to require function found with arguments "+lookupArgs(c, vargs))
      assert3(!(receiver contains UnknownValue), "call to unknown value found")
    case f:FunDecl =>
      c.env.put(f.v, new Fun(f))
    case ConditionalStatement(alt1, alt2) =>
      val c2 = c.split()
      a(c, alt1)
      a(c2, alt2)
      c.join(c2)
  }



  private def lookupArgs(c: Context, args: List[Variable]): String =
    args.map(c.env.lookup).map(_.mkString("[",",","]")).mkString(", ")


  private def assert3(c: Boolean, msg: String) =
    if (!c) throw new Analysis3Exception(msg)


}

class Analysis3Exception(msg: String) extends RuntimeException(msg)