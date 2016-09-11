package edu.cmu.cs.nodesec

import org.scalatest.FunSuite


/**
  * Created by ckaestne on 9/10/2016.
  */
class ParserTest extends FunSuite {

  val p = new JSParser()

  def ps(r: p.ParseResult[Any]): Unit = {
    println(r)
    r match {
      case p.Success(r, _) =>
      case p.NoSuccess(msg, rest) => fail("parsing failed: " + msg)
    }
  }

  test("simple parser test") {
    ps(p.parseAll(p.Statement, "break;"))
    ps(p.parseAll(p.StringLiteral, "'foo bar'"))
    ps(p.parseAll(p.StringLiteral, "\"foo bar\""))
    ps(p.parseAll(p.MemberExpression, "require('gulp-util')"))
    ps(p.parseAll(p.Expression, "a()()"))
    ps(p.parseAll(p.Expression, "process.env.INIT_CWD = process.cwd()"))
    ps(p.parseAll(p.Expression, "function(foo) {}"))
    ps(p.parseAll(p.Expression, "new Error(String(e.err)).stack"))
    ps(p.parseAll(p.Expression, "a(1)(2)"))
    ps(p.parseAll(p.Expression, "a.x1.x2.x3"))
    ps(p.parseAll(p.Expression, "a(1).foo(2).bar.bar2"))
    //    println(p.parseAll(p.word, "foo bar"))
  }

  test("parse leftpad") {
    execute("src/test/resources/leftpad.js")
  }
  test("parse gulp") {
    execute("src/test/resources/gulp.js")
  }

  test("parse demo") {
    execute("src/test/resources/demo.js")
  }


  def getSource(f: String) =
    io.Source.fromFile(f).getLines().map(
      l => if (l.contains("//")) l.take(l.indexOf("//")) else l
    ).map(
      l => if (l.startsWith("#!")) "" else l
    ).mkString("\n")


  def execute(f: String) = {
    val parsed = p.parseAll(p.Program, getSource(f))
    if (!parsed.successful) println(parsed)
    val prog = parsed.get

    for (s <- prog.inner)
      println(s)

    val env = Env.empty
    env.setVar("module", Value(ConcreteV(""), Set(Taint("MOD"))))
    env.setVar("require", Value(new RequireFunction(), Set(Taint("REQ"))))
    prog.execute(env)
    println(env)

    println("# exports")
    val exports = env.vars("module").members("exports")
    println(exports.v)
    exports.members.keys.map(println)

    def analyzeExported(name: String, f: V): Unit = {
      if (!f.isInstanceOf[FunctionV]) return
      val fun = f.asInstanceOf[FunctionV]
      println("# analyzing export "+name)
      val env = Env.empty
      val args = for (p <- fun.funExpr.param) yield
        new Value(new SymbolicV("param-" + p.a), Set(new Taint("IN-" + p.a)))
      val result = fun.call(env, args)
      if (result.taints.nonEmpty) System.err.println("returning result with taints "+result.taints)
    }
    analyzeExported("module.exports", exports.v)
    exports.members.map(a=>analyzeExported(a._1,a._2.v))


  }


  class RequireFunction extends FunctionV(FunExpr(None, Nil, EmptyStmt()), None) {
    override def call(env: Env, args: List[Value]): Value = {
      if (args.isEmpty) return new Value(Undefined())
      args.head.v match {
        case ConcreteV(s) =>
          new Value(new SymbolicV("module " + s), Set(new Taint("module:" + s)))
        case e =>
          System.err.println("cannot resolve `require` call statically")
          new Value(new SymbolicV("any module"), Set(new Taint("ANYMODULE")))
      }
    }

  }


}
