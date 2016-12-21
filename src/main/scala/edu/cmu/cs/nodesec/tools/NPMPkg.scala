package edu.cmu.cs.nodesec.tools

import java.io._

import edu.cmu.cs.nodesec.analysis.AnalysisHelper._
import edu.cmu.cs.nodesec.analysis.Policies._
import edu.cmu.cs.nodesec.analysis.{InferenceRules, MethodFactCollector}
import edu.cmu.cs.nodesec.datalog.Datalog
import edu.cmu.cs.nodesec.parser.JSASTParser

import scala.sys.process._

/**
  * Created by ckaestne on 11/27/16.
  */
object NPMPkg extends App {
  val pkg = "angular-frontload-data-3.0.0.tgz"


  val thisDir = new File(".")
  val dir = new File(thisDir, "npmlocal")
  val workingDir = new File(thisDir, "workingDir")
  val archive = new File(dir, pkg)

  //  val archives = dir.listFiles().filter(_.getName endsWith ".tgz")
  println(processPackage(archive))


  def processPackage(tgzFile: File): String = {
    println(s"## processing $tgzFile")

    Process(s"rm -r ${workingDir.getName()}", thisDir).!
    assert(!workingDir.exists())
    workingDir.mkdir()

    if (Process("tar -zxvf  " + tgzFile.getAbsolutePath, workingDir).! != 0)
      throw new RuntimeException("failed extraction\n")
    val jsFiles = Process("find . -name *.js", workingDir).!!.split("\n")
    if (jsFiles.isEmpty)
      throw new RuntimeException("no js files found\n")

    val p = new JSASTParser()
    val asts = for (jsFile <- jsFiles) yield {
      println(jsFile)
      val astFile = File.createTempFile("jast", "js")
      astFile.deleteOnExit()
      if (Process(s"node ../esprint/parse2.js $jsFile $astFile", workingDir).! != 0)
        throw new RuntimeException(s"pretty printing failed ($jsFile)\n")

      val parsed = p.parse(astFile, new File(workingDir,jsFile))
      (jsFile, parsed)
    }


    var cfgs = for ((jsFile, ast) <- asts) yield {
      (jsFile, cfgWithGlobals(ast))
    }


    for ((jsFile, fun) <- cfgs) {
      val facts = MethodFactCollector.collectFacts(fun)

      val d = new Datalog()
      InferenceRules.loadRules(d)
      d.load(facts.map(_._2).flatten)

      val policy = noCallToRequire
      val policyViolations = policy(d, fun)

      if (policyViolations.nonEmpty)
        throw new RuntimeException(s"policy violation found ($jsFile)\n")
    }


    ("succeeded\n")

  }


}
