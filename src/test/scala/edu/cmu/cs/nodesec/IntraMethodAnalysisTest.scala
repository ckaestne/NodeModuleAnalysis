package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._
import edu.cmu.cs.nodesec.datalog.{DFact, Datalog}
import edu.cmu.cs.nodesec.parser.JSParser
import org.scalatest.FunSuite

import scala.util.parsing.input.{NoPosition, Position}


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

  test("assignment") {
    reject("var x = require; \n x();")
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

  test("load / store") {
    reject("var x;\n" +
      "x.y=require;\n" +
      "x.y();")
    //store to undefined has no effect
    pass("var x;\n" +
      "x.y.z=require;\n" +
      "x.y.z();")
    pass("var x;\n" +
      "x.y.z.a=require;\n" +
      "x.y.z.a();")
    reject("var x={};\n" +
      "x.y={};" +
      "x.y.z=require;\n" +
      "x.y.z();")
    reject("var x={}; x.y={}; x.y.z={};\n" +
      "x.y.z.a=require;\n" +
      "x.y.z.a();")
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

  test("flow insensitive") {
    //we reject a number of correct programs
    reject(
      """
        |var x, y;
        |x = require;
        |x = y;
        |x();
      """.stripMargin
    )
    reject(
      """
        |var x, foo;
        |y = require;
        |if (x)
        | y = foo;
        |else y = bar;
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
        |x.x={};x.x.x={};x.x.x.x={};
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
  }


  //  test("leftpad") {
  //    passFile("src/test/resources/leftpad.js")
  //  }


  def reject(prog: String): Unit = {
    val violations = findNoRequireCallViolations(prog)
    assert(violations.nonEmpty, "expected policy violation, but none found")
    println("correctly found policy violation: " + violations.mkString("\n"))
  }

  private def findNoRequireCallViolations(prog: String) = {
    val vm = parse(prog)
    val fun = AnalysisHelper.cfgScript(vm)
    fun.body.nodes.toList.flatMap(_.s.reverse).foreach(println)
    val facts = MethodFactCollector.collectFacts(fun)

    for ((f, fact) <- facts) {
      println("%% function " + f.uniqueId)
      fact.map(_.toString).toList.sorted.foreach(println)
    }

    checkNoRequireCall(facts.map(_._2).flatten, fun)
  }

  private def checkNoRequireCall(facts: Iterable[DFact], fun: Fun): Seq[PolicyViolation] = {
    val d = new Datalog()
    InferenceRules.loadRules(d)
    d.load(facts)

    import Datalog.stripQuotes
    val rules =
      "%% query\n" +
        "forbiddenCallTarget(\"require-obj\").\n" +
        "stack(\"" + fun.uniqueId + "lv-require\", \"require-obj\").\n" +
        "callToForbiddenTarget(RV) :- invoke(F, TV, RV), stack(TV, O), forbiddenCallTarget(O)."
    println(d.loadRules(rules))
    val result = stripQuotes(d.query("callToForbiddenTarget", "X"))

    result.map(
      r => PolicyViolation("Potential call to 'require' found", getFunctionCallPositionByRetObj(r("X"), fun))
    )


  }

  private def getFunctionCallPositionByRetObj(retObjString: String, fun: Fun): Position = {
    val f = (fun.allInnerFunctions + fun).find(retObjString startsWith _.uniqueId)
    if (f.isDefined)
      f.get.body.nodes.flatMap(_.s).
        collect({ case c@Call(r, _, _, _) if fun.uniqueId + r == retObjString => c }).
        headOption.map(_.pos).getOrElse(NoPosition)
    else NoPosition
  }


  def pass(prog: String): Unit = {
    val violations = findNoRequireCallViolations(prog)
    assert(violations.isEmpty, "expected no policy violation, but found: " + violations.mkString("\n"))
    println("correctly found no policy violations")
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


}
