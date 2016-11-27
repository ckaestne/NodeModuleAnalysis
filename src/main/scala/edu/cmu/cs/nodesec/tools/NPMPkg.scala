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
object NPMPkg extends App {
  val pkg= "angulartics-webtrends-analytics-1.0.7.tgz"


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
      throw new RuntimeException ("failed extraction\n")
    val jsFiles = Process("find . -name *.js", workingDir).!!.split("\n")
    if (jsFiles.isEmpty)
      throw new RuntimeException ("no js files found\n")

    for (jsFile <- jsFiles) {
      println(jsFile)
      if (Process(s"node ../esprint/prettyprint.js $jsFile $jsFile.pp", workingDir).! != 0)
        throw new RuntimeException(s"pretty printing failed ($jsFile)\n")
    }

    val p = new JSParser()
    var asts = for (jsFile <- jsFiles) yield {
      val parsed = p.parseAll(p.Program, new FileReader(new File(workingDir, jsFile + ".pp")))
      println(parsed)
      assert(parsed.successful, parsed)
      (jsFile, parsed.get)
    }

    var vms = for ((jsFile, ast) <- asts) yield {
        (jsFile, wrapScript(ast))
    }


    for ((jsFile, fun) <- vms) {
      val policyViolations = new MethodCompositionAnalysis().
        analyze(AnalysisHelper.wrapWithGlobals(fun), noCallToRequire, Some(fun))
      if (policyViolations.nonEmpty)
        throw new RuntimeException (s"policy violation found ($jsFile)\n")
    }


    ("succeeded\n")

  }


}
