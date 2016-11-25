package edu.cmu.cs.nodesec

import org.scalatest.FunSuite

/**
  * Created by ckaestne on 11/25/16.
  */
class EnvTest extends FunSuite {

  val a = new Analysis3

  val emptyEnv = Env(Map(), Map(), Map(), new Obj())

  test("Env") {

    val v = new NamedVariable("v")
    val o1 = new Obj("o1")
    val o2 = new Obj("o2")
    val o3 = new Obj("o3")
    val x = emptyEnv.store(v, Set(o1))
    val y = emptyEnv.store(v, Set(o2))
    val l = Some(Load(v, v, "foo"))

    assert(x.union(y).lookup(v)._1 == Set(o1, o2))

    assert(x.union(emptyEnv).lookup(v)._1 == Set(o1))

    val xy = x.storeField(v, "foo", Set[Value](o2))
    val xz = x.storeField(v, "foo", Set[Value](o3))
    assert(xy.union(xz).lookupField(v, "foo", l)._1 == Set(o3, o2))

    assert(xy.union(x).lookupField(v, "foo", l)._1==Set(o2))


  }

}
