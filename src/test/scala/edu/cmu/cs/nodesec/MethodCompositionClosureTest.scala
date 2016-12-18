//package edu.cmu.cs.nodesec
//
//import edu.cmu.cs.nodesec.analysis.{AnalysisHelper, ExternalVariable, LocalVariable, MethodCompositionAnalysis}
//
///**
//  * Created by ckaestne on 12/11/16.
//  */
//class MethodCompositionClosureTest extends AbstractAnalysisTest {
//
//  test("single function") {
//    val fun = cfg("var x=3; foo=4;")
//    assert(fun.localVariables==Set(LocalVariable("x")))
//    assert(fun.closureVariables==Set(ExternalVariable("foo")))
//    val (ex,facts) = MethodCompositionAnalysis.composeWithInnerFunctions(fun)
//    assert(ex==Set(ExternalVariable("foo")))
////    facts.foreach(println)
//  }
//  test("basic closure") {
//    val fun = cfg("var x=3; function f(){x=4;}")
//    assert(fun.localVariables==Set(LocalVariable("x"),LocalVariable("f")))
//    assert(fun.closureVariables==Set())
//    val (ex,facts) = MethodCompositionAnalysis.composeWithInnerFunctions(fun)
//    assert(ex==Set())
////    facts.foreach(println)
//  }
//  test("propagate closure") {
//    val fun = cfg("var x=3; function f(){foo=4;}")
//    assert(fun.localVariables==Set(LocalVariable("x"),LocalVariable("f")))
//    assert(fun.closureVariables==Set())
//    val (ex,facts) = MethodCompositionAnalysis.composeWithInnerFunctions(fun)
//    assert(ex==Set(ExternalVariable("foo")))
////    facts.foreach(println)
//  }
//  test("nested closure") {
//    val fun = cfg("var x=3; function f(){function g(){x=3; y=4; z=5;} var y=5;}")
//    assert(fun.localVariables==Set(LocalVariable("x"),LocalVariable("f")))
//    assert(fun.closureVariables==Set())
//    val (ex,facts) = MethodCompositionAnalysis.composeWithInnerFunctions(fun)
//    assert(ex==Set(ExternalVariable("z")))
//    //    facts.foreach(println)
//  }
//
//
//
//
//  private def cfg(s: String) = AnalysisHelper.cfgScript(parse(s))
//
//
//}
