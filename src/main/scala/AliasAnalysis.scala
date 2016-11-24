package edu.cmu.cs.nodesec

class AliasAnalysis {


  //  heap-allocated objects and functions H, program variables
  //    V , call sites I, fields F, and integers Z.
  //
  trait HeapObject

  def freshObject: HeapObject = new Obj

  class Obj extends HeapObject {
    override def toString: String = "heap-" + this.hashCode()
  }

  //H
  // callsite I = Statement
  // field F = String
  // integer Z = Int
  // variables V = Variable


  //  CALLS(i : I,h : H) indicates when call site i invokes
  //  method h
  var calls: Set[(Statement, HeapObject)] = Set()

  //    FORMAL(h : H, z : Z, v : V ) records formal arguments of a
  //    function
  var formals: Set[(HeapObject, Int, Variable)] = Set()

  //  METHODRET(h : H, v : V ) records the return value of a
  //  method
  var methodRets: Set[(HeapObject, Variable)] = Set()

  //  ACTUAL(i : I,z : Z, v : V ) records actual arguments of a
  //    function call
  var actuals: Set[(Statement, Int, Variable)] = Set()

  //    CALLRET(i : I,v : V ) records the return value for a
  //  call site
  var callRets: Set[(Statement, Variable)] = Set()

  //    ASSIGN(v1 : V, v2 : V ) records variable assignments
  var assigns: Set[(Variable, Variable)] = Set()
  //    LOAD(v1 : V, v2 : V, f : F) represents field loads
  var loads: Set[(Variable, Variable, String)] = Set()
  //    STORE(v1 : V, f : F, v2 : V ) represents field stores
  var stores: Set[(Variable, Variable, String)] = Set()
  //    PTSTO(v : V,h : H) represents a points-to relation
  //  for variables
  var ptsTos: Set[(Variable, HeapObject)] = Set()
  //  HEAPPTSTO(h1 : H, f : F, h2 : H) represents a points-to relations
  //  for heap objects
  var heapPtsTos: Set[(HeapObject, String, HeapObject)] = Set()
  //    PROTOTYPE(h1 : H, h2 : H) records object prototypes
  var prototypes: Set[(HeapObject, HeapObject)] = Set()


  def analyze(s: Statement): Unit = {

    printRules()
    collectFacts(s)
  }


  def printRules(): Unit = {
    val rules =
      """pointsTo(V, H) :- alloc(V, H).
        |pointsTo(V, H) :- funcDecl(V, H).
        |pointsTo(V1, H) :- pointsTo(V2, H), assign(V1, V2).
        |directHeapStoreTo(H1,F, H2) :- store(V1,F,V2), pointsTo(V1, H1), pointsTo(V2, H2).
        |directHeapPointsTo(H1,F, H2) :- directHeapStoreTo(H1,F, H2).
        |pointsTo(V2, H2) :- load(V2, V1, F), pointsTo(V1, H1), heapPointsTo(H1,F, H2).
        |heapPointsTo(H1,F, H2) :- directHeapPointsTo(H1,F, H2).
        |% Call graph
        |calls(I, M) :- actual(I, this, C), pointsTo(C, M).
        |% Interprocedural assignments
        |assign(V1, V2) :- calls(I, M), formal(M, Z, V1), actual(I, Z, V2).
        |assign(V2, V1) :- calls(I, M), methodret(M, V1), callret(I, V2).
        |% Prototype handling
        |heapPointsTo(H1,F, H2) :- prototype(H1, H), heapPointsTo(H, F, H2).
        |%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%""".stripMargin
    println(rules)
  }

  def collectFacts(stmt: Statement): Unit = stmt match {

    case Sequence(s) => s.foreach(collectFacts)
    case Assignment(vl, vr) =>
      println(s"assign($vl, $vr).")
    case OpStatement(v, v1, v2) =>
      println(s"assign($v, $v1).")
      println(s"assign($v, $v2).")
    case PrimAssignment(_) =>
    case Return(v) =>
      println(s"callret($v).")
    case Constructor(v, v0, vps) =>
      //      v = new v0(v1, v2, ..., vn) PTSTO(v,dfresh ).
      //  PROTOTYPE(dfresh , h):– PTSTO(v0, m),
      //  HEAPPTSTO(m, "prototype", h).
      //  for z ∈ {1..n}, generate ACTUAL(i, z, vz).
      //  CALLRET(i, v).
      val o = freshObject
      val h = freshObject
      val m = freshObject
      println(s"pointsTo($v, $o).")
      println(s"prototype($o, $h) :- pointsTo($v0, $m), heapPointsTo($m, field-prototype, $h).")
      val i = "loc-" + stmt.hashCode()
      for (z <- 1 until vps.size)
        println(s"actual($i, $z, ${vps(z)}).")
      println(s"callret($i, $v).")

    case Call(v, v0, vthis, vargs) =>
      //  v = v0(vthis , v1, v2,...,vn) for z ∈ {1..n,this}, generate ACTUAL(i, z, vz).
      //  CALLRET(i, v).
      val i = "loc-" + stmt.hashCode()
      for (z <- 1 until vargs.size)
        println(s"actual($i, $z, ${vargs(z)}).")
      println(s"actual($i, this, $vthis).")
      println(s"callret($i, $v).")

    case Load(v1, v2, f) =>
      println(s"load($v1, $v2, field-$f).")
    case Store(v1, f, v2) =>
      println(s"store($v1, field-$f, $v2).")

    case FunDecl(v, vargs, body) =>
      //      v = function(v1, ..., vn) {s} PTSTO(v,dfresh ).
      //  HEAPPTSTO(dfresh , "prototype", pfresh ).
      //  FUNCDECL(dfresh ). PROTOTYPE(pfresh , hFP ).
      //  for z ∈ {1..n}, generate FORMAL(dfresh ,z,vz).
      //  METHODRET(dfresh , v).
      val o = freshObject
      val p = freshObject
      println(s"pointsTo($v, $o).")

      println(s"heapPointsTo($o, field-prototype, $p).")
      println(s"funcdecl($o).")
      println(s"prototype($p, prototype-function).")
      for (z <- 1 until vargs.size)
        println(s"formal($o, $z, ${vargs(z)}).")
      println(s"methodret($o, $v).")


      collectFacts(body)

  }


}
