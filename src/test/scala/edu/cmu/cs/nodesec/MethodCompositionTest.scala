package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._
import edu.cmu.cs.nodesec.parser.JSParser
import org.scalatest.FunSuite


/**
  * Created by ckaestne on 11/24/16.
  */
class MethodCompositionTest extends AbstractAnalysisTest {


  import MethodCompositionAnalysis._

  test("return and closure") {
    reject(
      """
        |function f() { return require; };
        |var x=f();
        |x();
      """.stripMargin, noCallToRequire
    )
  }

  test("return from closure with field") {
    reject(
      """
        |function f() {
        | var r={};
        | r.a=require;
        | return r; };
        |var x=f();
        |x.a();
      """.stripMargin, noCallToRequire
    )
  }

  test("arg") {
    reject(
      """
        |function f(x) { return x; };
        |var  x=f(require);
        |x();
      """.stripMargin, noCallToRequire
    )
  }

  test("arg2") {
    reject(
      """
        |function f(x) { function g(x) { return x; }; return g(x); };
        |var  x=f(require);
        |x();
      """.stripMargin, noCallToRequire
    )
  }


  test("fun arg") {
    reject(
      """
        |function f(xf) { return xf; };
        |function g(xg) { return xg; };
        |var x=f(g);
        |var y=x(require);
        |y();
      """.stripMargin, noCallToRequire
    )
  }

  test("reject because context insensitive") {
    reject(
      """
        |function f(xf) { return xf; };
        |var x=f(require);
        |var y=f(f);
        |y(3);
      """.stripMargin, noCallToRequire
    )
  }


  test("leftpad") {
    passFile("src/test/resources/leftpad.js", noCallToRequire)
  }

  test("closure") {
    reject("(function foo() { require(); })();", noCallToRequire)
    reject("function foo() { require(); };", noCallToRequire)
  }

  test("closure with variables") {
    //would be okay
    reject(
      """
        |var x = require;
        |function foo() {
        | x();
        |}
        |foo();
      """.stripMargin, noCallToRequire)
  }


  test("overapproximation of closure") {
    //would be okay
    reject(
      """
        |var x = require;
        |function foo() {
        | var x = function bar(){};
        | x();
        |}
        |foo();
      """.stripMargin, noCallToRequire)
  }

  test("writing to closure") {
    //make sure inner local variables do not leak outside
    pass(
      """
        |var x = function bar(){};;
        |function foo() {
        | var x = require;
        |}
        |foo();
        |x();
      """.stripMargin, noCallToRequire)
    reject(
      """
        |var x = function bar(){};;
        |function foo() {
        | x = require;
        |}
        |foo();
        |x();
      """.stripMargin, noCallToRequire)
  }

  ///////////////////////////////////////////////
  // other policies

  test("write to closure") {
    pass("var x; x=1;", noWriteToClosure)
    reject("x=1;", noWriteToClosure)
    reject("x.y.z=1;", noWriteToClosure)
    reject("var x=1; function foo() { x=2; }", noWriteToClosure)
    reject("function foo() { x=2; }", noWriteToClosure)
  }

  test("read from global") {
    pass("var x=1; return x;", noReadFromGlobal)
    reject("return x;", noReadFromGlobal)
    reject("function foo(){return x;}", noReadFromGlobal)
    reject("var x = 1; function foo(){return x;}", noReadFromGlobal) //analysis is fairly imprecise
  }

  test("no prototype") {
    pass("(function(){})(); var x = {};", noPrototype)
    reject("var x={}; x.prototype.foo=3;", noPrototype)
    reject("function foo(){}; var x=new foo(); x.prototype.foo=3;", noPrototype)
  }

  test("forbidden global objects") {
    pass("whatever(); require();", noForbiddenGlobalObjects)
    reject("eval(\"foo\");", noForbiddenGlobalObjects)
    reject("arguments(1)();", noForbiddenGlobalObjects)
    reject("x=eval; x();", noForbiddenGlobalObjects)
  }

  test("unresolved function calls") {
    pass("function foo(){} foo(); var x=foo; x();", noAlwaysUnresolvedFunctionCalls)
    reject("foo();", noAlwaysUnresolvedFunctionCalls)
    reject("require();", noAlwaysUnresolvedFunctionCalls)
    pass("var x; if (3) x=function(){}; x();", noAlwaysUnresolvedFunctionCalls) //cannot check absence of unresolved call in some cases
  }

}
