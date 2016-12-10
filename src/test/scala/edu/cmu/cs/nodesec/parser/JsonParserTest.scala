package edu.cmu.cs.nodesec.parser

import edu.cmu.cs.nodesec.analysis.AnalysisHelper
import org.scalatest.FunSuite

/**
  * Created by ckaestne on 11/27/16.
  */
class JsonParserTest extends FunSuite {

  val p = new JSASTParser()

  test("leftpad") {
    val content = io.Source.fromFile("src/test/resources/leftpad.js.ast").getLines().mkString("\n")
    val ast = p.parse(content)
    println(ast)
    println(AnalysisHelper.cfgScript(ast).innerFunctions.head.body.toDot)

  }

  test("gulp") {
    val content = io.Source.fromFile("src/test/resources/gulp.js.ast").getLines().mkString("\n")
    val ast = p.parse(content)
    println(ast)
    println(AnalysisHelper.cfgScript(ast))
  }

  test("parser") {
    val content = io.Source.fromFile("src/test/resources/angulartics-webtrends-analytics.js.ast").getLines().mkString("\n")
    println(p.parse(content))

  }

}
