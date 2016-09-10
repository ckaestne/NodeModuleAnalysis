package edu.cmu.cs.nodesec

import org.scalatest.FunSuite


/**
  * Created by ckaestne on 9/10/2016.
  */
class ParserTest extends FunSuite {

  val p = new JSParser()

  def ps(r:p.ParseResult[Any]): Unit = {
    println(r)
    r match {
      case p.Success(r, _) =>
      case p.NoSuccess(msg, rest) => fail("parsing failed: "+msg)
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
    //    println(p.parseAll(p.word, "foo bar"))
  }

  test("parse leftpad") {
    ps(p.parseAll(p.Program, getSource("src/test/resources/leftpad.js")))
  }
  test("parse gulp") {
    ps(p.parseAll(p.Program, getSource("src/test/resources/gulp.js")))
  }


  def getSource(f: String) =
    io.Source.fromFile(f).getLines().map(
      l => if (l.contains("//")) l.take(l.indexOf("//")) else l
    ).map(
      l => if (l.startsWith("#!")) "" else l
    ).mkString("\n")


}
