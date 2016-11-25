package edu.cmu.cs.nodesec

import org.scalatest.FunSuite


/**
  * Created by ckaestne on 11/24/16.
  */
class IntraMethodAnalysisTest extends FunSuite {

  test("direct call") {
    reject("require('foo');")
  }

  test("unknown call") {
    pass("bar('foo');")
  }

  test("no call") {
    pass("var x = require; var y = x;")
  }

  test("local call") {
    pass("var x = function() {};" +
      "x();")
    pass("function x() {};" +
      "x();")
    pass("x();" +
      "function x() {};")
  }

  test("undefined call") {
    //not a concern for us
    pass("var x;" +
      "x();")
  }


  test("if stmt") {
    reject(
      """
        |var x, y, foo;
        |if (x)
        | y = require;
        |else
        | y = foo;
        |y();
      """.stripMargin
    )
    reject(
      """
        |var x, foo;
        |if (x)
        | y = require;
        |y();
      """.stripMargin
    )
    pass(
      """
        |var x, y, foo;
        |if (x)
        | y = foo;
        |else
        | y = foo;
        |y();
      """.stripMargin
    )
    pass(
      """
        |var x, y, foo;
        |if (x)
        | y = foo;
        |y();
      """.stripMargin
    )
    pass(
      """
        |var x, foo;
        |if (x)
        | y = foo;
        |else
        | y = foo;
        |y();
      """.stripMargin
    )
  }

  test("loop") {
    reject(
      """
        |var x, y, foo;
        |while (x)
        | y = require;
        |y();
      """.stripMargin
    )
    pass(
      """
        |var x, y, foo;
        |while (x)
        | y = foo;
        |y();
      """.stripMargin
    )
    reject(
      """
        |var x, y, foo;
        |y = require;
        |while (x)
        | y = foo;
        |y();
      """.stripMargin
    )
    reject(
      """
        |var x, y, z;
        |x = require;
        |while (3) {
        | z = y;
        | y = x;
        |}
        |z();
      """.stripMargin
    )
    reject(
      """
        |var x={}, y, z;
        |x.x.x.x.x = require;
        |while (3) {
        | x=x.x;
        |}
        |x();
      """.stripMargin
    )
  }

  test("infinite loop") {
    pass(
      """
        |var x, y={};
        |while (x)
        | y = y.bar;
        |y();
      """.stripMargin
    )
  }

  test("fields") {
    reject(
      """
        |var x = {};
        |x.foo=require;
        |x.foo();
      """.stripMargin
    )
    pass(
      """
        |var x = {};
        |x.foo();
      """.stripMargin
    )
    pass(
      """
        |var x = {}, y;
        |x.foo=y;
        |x.foo();
      """.stripMargin
    )
    reject(
      """
        |var x, y;
        |if (3) x={}; else x={};
        |x.foo = y;
        |if (4) x.foo=require;
        |x.foo();
      """.stripMargin
    )
    reject(
      """
        |var x = {};
        |x.a.b.c.d.e=require;
        |x.a.b.c.d.e();
      """.stripMargin
    )
  }


  test("leftpad") {
    passFile("src/test/resources/leftpad.js")
  }


  def reject(prog: String): Unit = {
    val vm = parse(prog).toVM()
    printProg(vm)
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
    printProg(vm)
    new Analysis3().analyze(vm)
  }

  def passFile(file: String): Unit = {
    val vm = parseFile(file).toVM()
    printProg(vm)
    new Analysis3().analyze(vm)
  }

  def parse(prog: String) = {
    val parsed = p.parseAll(p.Program, prog)
    if (!parsed.successful) println(parsed)
    parsed.get
  }
  def parseFile(file: String) = {
    val parsed = p.parseAll(p.Program, getSource(file))
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

  def printProg(s: Statement, indent: Int = 0): Unit = {
    val in = "  " * indent
    s match {
      case Sequence(inner) => inner.reverse.map(printProg(_, indent))
      case FunDecl(v, args, body) =>
        println(in + s"FunDecl $v, $args:")
        printProg(body, indent + 1)
      case ConditionalStatement(a, b) =>
        println(in + s"if:")
        printProg(a, indent + 1)
        println(in + "else:")
        printProg(b, indent + 1)
      case LoopStatement(a) =>
        println(in + s"loop:")
        printProg(a, indent + 1)
      case _ => println(in + s.toString)
    }
  }

}
