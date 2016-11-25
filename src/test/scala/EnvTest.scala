package edu.cmu.cs.nodesec

import org.scalatest.FunSuite

/**
  * Created by ckaestne on 11/25/16.
  */
class EnvTest extends FunSuite {

  val a = new Analysis3

  val emptyEnv = a.Env(Map(), Map(), new a.Obj(), None)

  test("Env") {

    val v = new NamedVariable("v")
    val o1 = new a.Obj("o1")
    val o2 = new a.Obj("o2")
    val o3 = new a.Obj("o3")
    val x = emptyEnv.store(v, Set(o1))
    val y = emptyEnv.store(v, Set(o2))

    assert(x.union(y).lookup(v)._1 == Set(o1, o2))

    assert(x.union(emptyEnv).lookup(v)._1.contains(o1))
    assert(x.union(emptyEnv).lookup(v)._1.exists(_.isUnknown))

    val xy = x.storeField(v, "foo", Set[a.Value](o2))
    val xz = x.storeField(v, "foo", Set[a.Value](o3))
    assert(xy.union(xz).lookupField(v,"foo")._1 == Set(o3, o2))

    assert(xy.union(x).lookupField(v,"foo")._1.contains(o2))
    assert(xy.union(x).lookupField(v,"foo")._1.exists(_.isUnknown))




  }

}
