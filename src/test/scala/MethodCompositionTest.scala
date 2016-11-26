package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._
import edu.cmu.cs.nodesec.parser.JSParser
import org.scalatest.FunSuite


/**
  * Created by ckaestne on 11/24/16.
  */
class MethodCompositionTest extends FunSuite {


  test("return") {
    reject(
      """
        |function f() { return require; };
        |var x=f();
        |x();
      """.stripMargin
    )
  }

  test("return with field") {
    reject(
      """
        |function f() {
        | var r={};
        | r.a=require;
        | return r; };
        |var x=f();
        |x.a();
      """.stripMargin
    )
  }

  test("arg") {
    reject(
      """
        |function f(x) { return x; };
        |var  x=f(require);
        |x();
      """.stripMargin
    )
  }

  test("arg2") {
    reject(
      """
        |function f(x) { function g(x) { return x; }; return g(x); };
        |var  x=f(require);
        |x();
      """.stripMargin
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
      """.stripMargin
    )
  }

  test("reject because context insensitive") {
    reject(
      """
        |function f(xf) { return xf; };
        |var x=f(require);
        |var y=f(f);
        |y(3);
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
      checkPolicy(new MethodCompositionAnalysis().analyzeScript(vm))
      //      fail("expected to reject program, but passed")
    } catch {
      case e: Analysis3Exception =>
        println("rejected: " + e.getMessage)
    }
  }

  def pass(prog: String): Unit = {
    val vm = parse(prog).toVM()
    printProg(vm)
    checkPolicy(new MethodCompositionAnalysis().analyzeScript(vm))
  }


  def passFile(file: String): Unit = {
    val vm = parseFile(file).toVM()
    printProg(vm)
    checkPolicy(new MethodCompositionAnalysis().analyzeScript(vm))
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
