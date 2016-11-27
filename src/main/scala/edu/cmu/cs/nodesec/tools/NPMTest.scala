package edu.cmu.cs.nodesec.tools


import java.io._

import edu.cmu.cs.nodesec.analysis.AnalysisHelper._
import edu.cmu.cs.nodesec.analysis.MethodCompositionAnalysis.noCallToRequire
import edu.cmu.cs.nodesec.analysis.{AnalysisHelper, MethodCompositionAnalysis}
import edu.cmu.cs.nodesec.parser.JSParser

import scala.sys.process._

/**
  * Created by ckaestne on 11/27/16.
  */
object NPMTest extends App {

  val thisDir = new File(".")
  val dir = new File(thisDir, "npmlocal")
  val workingDir = new File(thisDir, "workingDir")
  val log = new FileWriter(new File(dir, "log"))

//  val archives = dir.listFiles().filter(_.getName endsWith ".tgz")
  val archives = List(new File(dir, "angulartics-webtrends-analytics-1.0.7.tgz"))
  archives.foreach(processPackage)

  log.close()

  def processPackage(tgzFile: File): Any = {
    println(s"## processing $tgzFile")
    log.flush()
    log.append(tgzFile.getName).append(": ")

    Process(s"rm -r ${workingDir.getName()}", thisDir).!
    assert(!workingDir.exists())
    workingDir.mkdir()

    if (Process("tar -zxvf  " + tgzFile.getAbsolutePath, workingDir).! != 0)
      return log.append("failed extraction\n")
    val jsFiles = Process("find . -name *.js", workingDir).!!.split("\n")
    if (jsFiles.isEmpty)
      return log.append("no js files found\n")

    for (jsFile <- jsFiles) {
      println(jsFile)
      if (Process(s"node ../esprint/prettyprint.js $jsFile $jsFile.pp", workingDir).! != 0)
        return log.append(s"pretty printing failed ($jsFile)\n")
    }

    val p = new JSParser()
    var asts = for (jsFile <- jsFiles) yield {
      val parsed = p.parseAll(p.Program, new FileReader(new File(workingDir, jsFile + ".pp")))
      if (!parsed.successful)
        return log.append(s"parsing failed ($jsFile)\n")
      (jsFile, parsed.get)
    }

    var vms = for ((jsFile, ast) <- asts) yield {
      try {
        (jsFile, wrapScript(ast))
      } catch {
        case e: scala.NotImplementedError => return log.append(s"toVM failed ($jsFile)\n")
      }
    }


    for ((jsFile, fun) <- vms) {
      val policyViolations = new MethodCompositionAnalysis().
        analyze(AnalysisHelper.wrapWithGlobals(fun), noCallToRequire, Some(fun))
      if (policyViolations.nonEmpty)
        return log.append(s"policy violation found ($jsFile)\n")
    }


    log.append("succeeded\n")

  }


}
