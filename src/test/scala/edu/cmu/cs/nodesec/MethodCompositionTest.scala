package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._
import edu.cmu.cs.nodesec.parser.JSParser
import org.scalatest.FunSuite


/**
  * Created by ckaestne on 11/24/16.
  */
class MethodCompositionTest extends FunSuite {


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
  // policy: no write to external objects

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
    reject("val x = 1; function foo(){return x;}", noReadFromGlobal) //analysis is fairly imprecise
  }

  test("no prototype") {
    pass("(function(){})(); var x = {};",noPrototype)
    reject("var x={}; x.prototype.foo=3;",noPrototype)
    reject("function foo(){}; var x=new foo(); x.prototype.foo=3;",noPrototype)
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
    pass("var x; if (3) x=function(){}; x();", noAlwaysUnresolvedFunctionCalls)//cannot check absence of unresolved call in some cases
  }

  def reject(prog: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val vm = parse(prog)
    val policyViolations = new MethodCompositionAnalysis().analyzeScript(vm, policies.reduce(_ + _))
    println(policyViolations.map(_.render).mkString("\n"))
    assert(policyViolations.nonEmpty, "policy violation expected, but not found")
  }

  def pass(prog: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val vm = parse(prog)
    val policyViolations = new MethodCompositionAnalysis().analyzeScript(vm, policies.reduce(_ + _))
    assert(policyViolations.isEmpty, "policy violation found:\n" + policyViolations.map(_.render).mkString("\n"))
  }


  def passFile(file: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val vm = parseFile(file)
    val policyViolations = new MethodCompositionAnalysis().analyzeScript(vm, policies.reduce(_ + _))
    assert(policyViolations.isEmpty, "policy violation found:\n" + policyViolations.map(_.render).mkString("\n"))
  }

  def checkPolicy(env: Env): Unit = {
    //    for ((call, paramset) <- env.calls;
    //         params <- paramset;
    //         target <- params.head)
    //      if (target == Param("require"))
    //        throw new Analysis3Exception("call to require function found -- " + call)
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

  //  def printProg(s: Statement, indent: Int = 0): Unit = {
  //    val in = "  " * indent
  //    s match {
  //      case Sequence(inner) => inner.reverse.map(printProg(_, indent))
  //      case FunDecl(v, args, body) =>
  //        println(in + s"FunDecl $v, $args:")
  //        printProg(body, indent + 1)
  //      case ConditionalStatement(a, b) =>
  //        println(in + s"if:")
  //        printProg(a, indent + 1)
  //        println(in + "else:")
  //        printProg(b, indent + 1)
  //      case LoopStatement(a) =>
  //        println(in + s"loop:")
  //        printProg(a, indent + 1)
  //      case _ => println(in + s.toString)
  //    }
  //  }

}
