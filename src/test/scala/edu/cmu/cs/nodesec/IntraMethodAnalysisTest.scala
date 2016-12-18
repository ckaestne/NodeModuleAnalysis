package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._


/**
  * Created by ckaestne on 11/24/16.
  */
class IntraMethodAnalysisTest extends AbstractAnalysisTest {


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
    val violations = checkPolicy(prog, Policies.noCallToRequire)
    assert(violations.nonEmpty, "expected policy violation, but none found")
    println("correctly found policy violation: " + violations.mkString("\n"))
  }



  def pass(prog: String): Unit = {
    val violations = checkPolicy(prog, Policies.noCallToRequire)
    assert(violations.isEmpty, "expected no policy violation, but found: " + violations.mkString("\n"))
    println("correctly found no policy violations")
  }


}
