package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis.Policies._

/**
  * Created by ckaestne on 11/24/16.
  */
class PolicyTest extends AbstractAnalysisTest {

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


//  test("leftpad") {
//    passFile("src/test/resources/leftpad.js", noCallToRequire)
//  }

  test("closure") {
    reject("(function foo() { require(); })();", noCallToRequire)
    reject("function foo() { require(); };", noCallToRequire)
  }

  test("closure with variables") {
    //would be okay, but flow-insensitive analysis
    reject(
      """
        |var x = require;
        |function foo() {
        | x();
        |}
        |foo();
      """.stripMargin, noCallToRequire)
  }


  test("scoping check") {
    reject(
      """
        |var x = require;
        |function foo() {
        | x = function bar(){};
        | x();
        |}
        |foo();
      """.stripMargin, noCallToRequire)
    pass(
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
    reject(
      """
        |var x = function bar(){};;
        |function foo() {
        | x = require;
        |}
        |x();
      """.stripMargin, noCallToRequire)
    //not flow sensitive
    reject(
      """
        |var x = function bar(){};;
        |function a() {
        | x = require;
        |}
        |function b() {
        | x = 3;
        |}
        |a();
        |b();
        |x();
      """.stripMargin, noCallToRequire)
  }

//  ///////////////////////////////////////////////
//  // other policies
//
  test("write to closure") {
    pass("var x; x=1;", noWriteToClosure)
    reject("x=1;", noWriteToClosure)
    pass("var x=1; function foo() { x=2; }", noWriteToClosure)
    reject("function foo() { x=2; }", noWriteToClosure)
  }

  test("store to object from closure") {
    pass("var x={}; x.x=1;", noStoreToClosure)
    reject("x.y=1;", noStoreToClosure)
    reject("function f(){ x.y=1; }", noStoreToClosure)
    reject("var a=x; a.y=1;", noStoreToClosure)
    reject("x.y.z=1;", noStoreToClosure)
    reject("var a= x.y; a.z=1;", noStoreToClosure)
    pass("var a={}; a.b=x.y;foo(a.b.z);", noStoreToClosure)
    reject("var a={}; a.b=x.y; a.b.z=1;", noStoreToClosure)
    reject("var a={}; a.b.c=x.y; a.b.c.z=1;", noStoreToClosure)
    //module is a local parameter, not a global object
    pass("module.foo=3;", noStoreToClosure)
  }

  test("read from global") {
    pass("var x=1; return x;", noReadFromClosure)
    reject("return x;", noReadFromClosure)
    reject("return x.f;", noReadFromClosure)
    reject("var y; y.x=x;", noReadFromClosure)
    reject("function f(){return x;}", noReadFromClosure)
    reject("function foo(){return x;}", noReadFromClosure)
    pass("var x = 1; function foo(){return x;}", noReadFromClosure)
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
