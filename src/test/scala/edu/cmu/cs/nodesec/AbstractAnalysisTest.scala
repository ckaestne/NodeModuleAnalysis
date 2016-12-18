package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._
import edu.cmu.cs.nodesec.datalog.Datalog
import edu.cmu.cs.nodesec.parser.JSParser
import org.scalatest.FunSuite


/**
  * Created by ckaestne on 11/24/16.
  */
abstract class AbstractAnalysisTest extends FunSuite {




  def reject(prog: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val policyViolations: Seq[PolicyViolation] = checkPolicy(prog, policies.reduce(_ + _))

    println(policyViolations.map(_.render).mkString("\n"))
    assert(policyViolations.nonEmpty, "policy violation expected, but not found")
  }



  def pass(prog: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val policyViolations: Seq[PolicyViolation] = checkPolicy(prog, policies.reduce(_ + _))
    assert(policyViolations.isEmpty, "policy violation found:\n" + policyViolations.map(_.render).mkString("\n"))
  }


//  def passFile(file: String, policies: Policy*): Unit = {
//    assert(policies.nonEmpty, "no policies provided")
//    val vm = parseFile(file)
//    val policyViolations = MethodCompositionAnalysis.analyzeScript(vm, policies.reduce(_ + _), true)
//    assert(policyViolations.isEmpty, "policy violation found:\n" + policyViolations.map(_.render).mkString("\n"))
//  }
//
//  def rejectFile(file: String, policies: Policy*): Unit = {
//    assert(policies.nonEmpty, "no policies provided")
//    val vm = parseFile(file)
//    val policyViolations = MethodCompositionAnalysis.analyzeScript(vm, policies.reduce(_ + _), true)
//    assert(policyViolations.nonEmpty, "policy violation expected, but not found")
//  }


  def parse(prog: String) = {
    val parsed = p.parseAll(p.Program, prog)
    assert(parsed.successful, "parsing failed: " + parsed)
    parsed.get
  }

  def parseFile(file: String) = {
    val parsed = p.parseAll(p.Program, getSource(file))
    if (!parsed.successful) println(parsed)
    parsed.get
  }


  val p = new JSParser()


  def getSource(f: String) =
    io.Source.fromFile(f).getLines().map(
      l => if (l.contains("//")) l.take(l.indexOf("//")) else l
    ).map(
      l => if (l.startsWith("#!")) "" else l
    ).mkString("\n")


  def checkPolicy(prog: String, policy: Policy): Seq[PolicyViolation] = {
    val vm = parse(prog)
    val fun = AnalysisHelper.cfgScript(vm)
    fun.body.nodes.toList.flatMap(_.s.reverse).foreach(println)
    val facts = MethodFactCollector.collectFacts(fun)

    for ((f, fact) <- facts) {
      println("%% function " + f.uniqueId)
      fact.map(_.toString).toList.sorted.foreach(println)
    }

    val d = new Datalog()
    InferenceRules.loadRules(d)
    d.load(facts.map(_._2).flatten)

    val policyViolations = policy(d, fun)
    policyViolations
  }

}
