package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._
import edu.cmu.cs.nodesec.datalog.Datalog
import edu.cmu.cs.nodesec.parser.{FunctionBody, JSParser}
import org.scalatest.FunSuite


/**
  * Created by ckaestne on 11/24/16.
  */
abstract class AbstractAnalysisTest extends FunSuite {


  def reject(prog: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val policyViolations: Seq[PolicyViolation] = checkPolicy(parse(prog), policies.reduce(_ + _))

    assert(policyViolations.nonEmpty, "policy violation expected, but not found")
    println(policyViolations.map(_.render).mkString("\n"))
  }


  def pass(prog: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val policyViolations: Seq[PolicyViolation] = checkPolicy(parse(prog), policies.reduce(_ + _))

    assert(policyViolations.isEmpty, "policy violation found:\n" + policyViolations.map(_.render).mkString("\n"))
  }


  def passFile(file: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val policyViolations: Seq[PolicyViolation] = checkPolicy(parseFile(file), policies.reduce(_ + _))

    assert(policyViolations.isEmpty, "policy violation found:\n" + policyViolations.map(_.render).mkString("\n"))
    println(policyViolations.map(_.render).mkString("\n"))
  }

  def rejectFile(file: String, policies: Policy*): Unit = {
    assert(policies.nonEmpty, "no policies provided")
    val policyViolations: Seq[PolicyViolation] = checkPolicy(parseFile(file), policies.reduce(_ + _))

    assert(policyViolations.nonEmpty, "policy violation expected, but not found")
  }


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


  def checkPolicy(ast: FunctionBody, policy: Policy): Seq[PolicyViolation] = {
    val fun = AnalysisHelper.cfgScript(ast)
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
