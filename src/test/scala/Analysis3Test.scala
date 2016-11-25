package edu.cmu.cs.nodesec

import org.scalatest.FunSuite


/**
  * Created by ckaestne on 11/24/16.
  */
class Analysis3Test extends FunSuite {

  test("direct call") {
    reject("require('foo');")
  }

  test("unknown call") {
    reject("bar('foo');")
  }

  test("no call") {
    pass("var x = require; var y = x;")
  }

  test("local call") {
    pass("var x = function() {};" +
      "x();")
  }



  def reject(prog: String): Unit = {
    val vm = parse(prog).toVM()
    try {
      new Analysis3().analyze(vm)
      fail("expected to reject program, but passed")
    } catch {
      case e: Analysis3Exception =>
        println("rejected: " + e.getMessage)
    }
  }

  def pass(prog: String): Unit = {
    val vm = parse(prog).toVM()
    new Analysis3().analyze(vm)
  }

  def parse(prog: String) = {
    val parsed = p.parseAll(p.Program, prog)
    if (!parsed.successful) println(parsed)
    parsed.get
  }


  val p = new JSParser()

  def ps(r: p.ParseResult[Any]): Unit = {
    println(r)
    r match {
      case p.Success(_, _) =>
      case p.NoSuccess(msg, rest) => fail("parsing failed: " + msg)
    }
  }


  def getSource(f: String) =
    io.Source.fromFile(f).getLines().map(
      l => if (l.contains("//")) l.take(l.indexOf("//")) else l
    ).map(
      l => if (l.startsWith("#!")) "" else l
    ).mkString("\n")


}
