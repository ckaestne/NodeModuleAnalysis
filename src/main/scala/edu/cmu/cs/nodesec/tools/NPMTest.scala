package edu.cmu.cs.nodesec.tools


import java.io._

import edu.cmu.cs.nodesec.analysis.AnalysisHelper._
import edu.cmu.cs.nodesec.analysis.{InferenceRules, MethodFactCollector}
import edu.cmu.cs.nodesec.analysis.Policies._
import edu.cmu.cs.nodesec.datalog.Datalog
import edu.cmu.cs.nodesec.parser.JSONParser.{JsObject, JsString, JsValue}
import edu.cmu.cs.nodesec.parser.{JSASTParser, JSONParser, JSParser}

import scala.sys.process._

/**
  * Created by ckaestne on 11/27/16.
  */
object NPMTest extends App {

  val thisDir = new File(".")
  val dir = new File(thisDir, "npmlocal")
  val workingDir = new File(thisDir, "workingDir")
  val log = new FileWriter(new File(dir, "log")) {
    override def append(charSequence: CharSequence): Writer = { System.err.print(charSequence); super.append(charSequence)}
  }

  val archives = dir.listFiles().filter(_.getName endsWith ".tgz").drop(29).take(1)
  //  val archives = List(new File(dir, "angulartics-webtrends-analytics-1.0.7.tgz"))
  archives.foreach(processPackage)

  log.close()

  def lookupMainFile(pkgjson: File): Option[String] =
    JSONParser.parseJSON(pkgjson).child("main").flatMap(getText)

  private def getText(v: JsValue): Option[String] = v match {
    case JsString(s) => Some(s)
    case _ => None
  }

  def processPackage(tgzFile: File): Any = {
    println(s"## processing $tgzFile")
    log.flush()
    log.append(tgzFile.getName).append(": ")

    Process(s"rm -r ${workingDir.getName()}", thisDir).!
    assert(!workingDir.exists())
    workingDir.mkdir()

    if (Process("tar -zxvf  " + tgzFile.getAbsolutePath, workingDir).! != 0)
      return log.append("failed extraction\n")
    var jsFiles = Process("find . -name *.js", workingDir).!!.split("\n").toSeq
    if (jsFiles.isEmpty)
      return log.append("no js files found\n")

    val pkgjson=new File(workingDir, "package/package.json")
    if (pkgjson.exists()) {
      var mainFile = lookupMainFile(pkgjson)
      if (mainFile.isDefined) {
        if (!new File(workingDir, "package/" + mainFile.get).exists())
          if (!mainFile.get.toLowerCase().endsWith(".js"))
            mainFile = Some(mainFile.get+".js")
        if (!new File(workingDir, "package/" + mainFile.get).exists())
          return log.append(s"main file ${mainFile.get} not found\n")
        jsFiles = "package/" + mainFile.get :: Nil
      }
    }

    val p = new JSASTParser()
    val asts = for (jsFile <- jsFiles) yield {
      println(jsFile)
      val astFile = File.createTempFile("jast", "js")
      astFile.deleteOnExit()
      if (Process(s"node ../esprint/parse2.js $jsFile $astFile", workingDir).! != 0)
        return log.append(s"esprima failed ($jsFile)\n")

      try {
        val parsed = p.parse(astFile, new File(workingDir, jsFile))
        (jsFile, parsed)
      } catch {
        case e: MatchError => return log.append(s"parsing failed ($jsFile): $e\n")
        case e: scala.NotImplementedError => return log.append(s"parsing failed ($jsFile): $e\n")
        case e: AssertionError => return log.append(s"parsing failed ($jsFile): $e\n")
      }
    }

    for (jsFile <- jsFiles) {
      println(jsFile)
      if (Process(s"node ../esprint/prettyprint.js $jsFile $jsFile.pp", workingDir).! != 0)
        return log.append(s"pretty printing failed ($jsFile)\n")
    }

    //    val p = new JSParser()
    //    var asts = for (jsFile <- jsFiles) yield {
    //      try {
    //        val parsed = p.parseAll(p.Program, new FileReader(new File(workingDir, jsFile + ".pp")))
    //        if (!parsed.successful)
    //          return log.append(s"parsing failed ($jsFile)\n")
    //        (jsFile, parsed.get)
    //      } catch {
    //        case e: scala.NotImplementedError => return log.append(s"parsing failed ($jsFile)\n")
    //        case e: AssertionError => return log.append(s"parsing failed ($jsFile)\n")
    //      }
    //
    //    }

    var cfgs = for ((jsFile, ast) <- asts) yield {
      try {
        (jsFile, cfgWithGlobals(ast))
      } catch {
        case e: scala.NotImplementedError => return log.append(s"cfg computation failed ($jsFile): $e\n")
        case e: AssertionError => return log.append(s"cfg computation failed ($jsFile): $e\n")
      }
    }


    for ((jsFile, fun) <- cfgs) {
      try {
        val facts = MethodFactCollector.collectFacts(fun)

        val d = new Datalog()
        InferenceRules.loadRules(d)
        d.load(facts.map(_._2).flatten)

        val policy = noCallToRequire
        val policyViolations = policy(d, fun)

        if (policyViolations.nonEmpty) {
          policyViolations.map(_.render).foreach(System.err.println)
          return log.append(s"policy violation found ($jsFile)\n")
        }

      } catch {
        case e: scala.NotImplementedError => return log.append(s"policy checking failed ($jsFile)\n")
        case e: AssertionError => return log.append(s"policy checking failed ($jsFile)\n")
      }
    }


    log.append("succeeded\n")

  }


}
