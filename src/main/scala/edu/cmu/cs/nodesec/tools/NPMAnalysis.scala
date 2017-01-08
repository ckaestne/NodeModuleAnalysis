package edu.cmu.cs.nodesec.tools

import java.io._

import edu.cmu.cs.nodesec.analysis.AnalysisHelper._
import edu.cmu.cs.nodesec.analysis.Policies._
import edu.cmu.cs.nodesec.analysis.{InferenceRules, MethodFactCollector}
import edu.cmu.cs.nodesec.datalog.Datalog
import edu.cmu.cs.nodesec.parser.JSONParser.{JsString, JsValue}
import edu.cmu.cs.nodesec.parser.{JSASTParser, JSONParser}
import edu.cmu.cs.nodesec.tools.DB.Result

import scala.sys.process._

/**
  * pick an unanalyzed package from the database, download and analyze it, store results
  */
object NPMAnalysis extends App {

  val thisDir = new File(".")
  val workingDir = new File(thisDir, "workingDir")
  val tarFile = new File(workingDir, "pkg.tgz")

  while (true) {

    val pkg = DB.grabNextPackage()

    if (!pkg.isDefined) {
      println("nothing to analyze... waiting.")
      Thread.sleep(3333)
    } else {
      DB.reportResult(processPackageName(pkg.get._1, pkg.get._2))
    }

  }


  def lookupMainFile(pkgjson: File): Option[String] =
    JSONParser.parseJSON(pkgjson).child("main").flatMap(getText)

  private def getText(v: JsValue): Option[String] = v match {
    case JsString(s) => Some(s)
    case _ => None
  }


  def processPackageName(name: String, version: String): Result = {
    println("processing " + name)
    //clean working directory
    Process(s"rm -r ${workingDir.getName()}", thisDir).!
    assert(!workingDir.exists())
    workingDir.mkdir()


    val tarball = s"npm view $name@$version dist.tarball".!!

//    //lookup and download latest version
//    val json = try {
//      val url = new URL("https://registry.npmjs.org/" + name + "/")
//
//      JSONParser.parseJSON(Channels.newChannel(url.openStream()))
//    } catch {
//      case e: jawn.ParseException => return Result(name, version, 10, e.getMessage)
//    }
//
////    val version = json.child("dist-tags").get.child("latest").get.asInstanceOf[JsString].v
//    try {
//      val versiondesc = json.child("versions").get.child(version).get
//      val tarball = versiondesc.child("dist").get.child("tarball").get.asInstanceOf[JsString].v

    try {
      println(s"downloading $tarball")
      if (("curl --max-filesize 15000 " + tarball).#>(tarFile).! != 0)
        return Result(name, version, 13, "downloading tar aborted")

    } catch {
      case e: Exception =>
        e.printStackTrace()
        return Result(name, version, 10, e.getMessage)
    }



    processPackage(name, version, tarFile)
  }


  def processPackage(name: String, version: String, tgzFile: File): Result = {
    println(s"## processing $tgzFile")


    if (Process("tar -zxvf  " + tgzFile.getAbsolutePath, workingDir).! != 0)
      return Result(name, version, 20, "failed extraction")
    var jsFiles = Process("find . -name *.js", workingDir).!!.split("\n").toSeq
    if (jsFiles.isEmpty)
      return Result(name, version, 30, "no js files found\n")

    val pkgjson = new File(workingDir, "package/package.json")
    if (pkgjson.exists()) {
      var mainFile = lookupMainFile(pkgjson)
      if (mainFile.isDefined) {
        if (!new File(workingDir, "package/" + mainFile.get).exists())
          if (!mainFile.get.toLowerCase().endsWith(".js"))
            mainFile = Some(mainFile.get + ".js")
        if (!new File(workingDir, "package/" + mainFile.get).exists())
          return Result(name, version, 31, s"main file ${mainFile.get} not found\n")
        jsFiles = "package/" + mainFile.get :: Nil
      }
    }

    val p = new JSASTParser()
    val asts = for (jsFile <- jsFiles) yield {
      println(jsFile)
      val astFile = File.createTempFile("jast", "js")
      astFile.deleteOnExit()
      if (Process(s"node ../esprint/parse2.js $jsFile $astFile", workingDir).! != 0)
        return Result(name, version, 40, s"esprima failed ($jsFile)\n")

      try {
        val parsed = p.parse(astFile, new File(workingDir, jsFile))
        (jsFile, parsed)
      } catch {
        case e: java.nio.charset.MalformedInputException => return Result(name, version, 40, s"parsing failed ($jsFile): $e\n")
        case e: MatchError => return Result(name, version, 40, s"parsing failed ($jsFile): $e\n")
        case e: scala.NotImplementedError => return Result(name, version, 40, s"parsing failed ($jsFile): $e\n")
        case e: AssertionError => return Result(name, version, 40, s"parsing failed ($jsFile): $e\n")
      }
    }

    for (jsFile <- jsFiles) {
      println(jsFile)
      if (Process(s"node ../esprint/prettyprint.js $jsFile $jsFile.pp", workingDir).! != 0)
        return Result(name, version, 40, s"pretty printing failed ($jsFile)\n")
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
        case e: scala.NotImplementedError => return Result(name, version, 50, s"cfg computation failed ($jsFile): $e\n")
        case e: AssertionError => return Result(name, version, 50, s"cfg computation failed ($jsFile): $e\n")
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
          return Result(name, version, 51, s"policy violation found ($jsFile)\n")
        }

      } catch {
        case e: scala.NotImplementedError => return Result(name, version, 50, s"policy checking failed ($jsFile)\n")
        case e: AssertionError => return Result(name, version, 50, s"policy checking failed ($jsFile)\n")
      }
    }


    Result(name, version, 1, "no policy violation found")
  }


}
