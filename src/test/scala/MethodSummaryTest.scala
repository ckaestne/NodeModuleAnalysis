package edu.cmu.cs.nodesec

import edu.cmu.cs.nodesec.analysis._
import edu.cmu.cs.nodesec.parser.JSParser
import org.scalatest.FunSuite


/**
  * Created by ckaestne on 11/24/16.
  */
class MethodSummaryTest extends FunSuite {

  test("paper") {
    pass(
      """
        |var o={}, t={}, end = 0;
        |var p = t;
        |while (3) {
        |  if (o.equals(p.data)) end = 1;
        |  if (!end) {
        |   if (p.next != null)
        |     p = p.next;
        |  }
        |}
        |if (!end) {
        |  p.next = {};
        |  p.next.data = o;
        |}
        |return true;
      """.stripMargin)
  }

  test("summary1") {
    pass(
      """
        |var z = {};
        |a=bar();
        |z.z=3;
        |var x = foo.bar;
        |var y = x()(z.z);
        |return y;
      """.
        stripMargin
    )
  }




  test("leftpad") {
    passFile("src/test/resources/leftpad.js")
  }


  val analysis3 = new IntraMethodAnalysis()

  def reject(prog: String): Unit = {
    //TODO do nothing for now
  }

  def pass(prog: String): Unit = {
    val vm = parse(prog).toVM()
    printProg(vm)
    val env = analysis3.analyzeScript(vm)
    printEnv(env)
  }

  def passFile(file: String): Unit = {
    val vm = parseFile(file).toVM()
    printProg(vm)
    analysis3.analyzeScript(vm)
  }

  def parse(prog: String) = {
    val parsed = p.parseAll(p.Program, prog)
    if (!parsed.successful) println(parsed)
    parsed.get
  }

  def parseFile(file: String) = {
    val parsed = p.parseAll(p.Program, getSource(file))
    if (!parsed.successful) println(parsed)
    parsed.get
  }


  val p = new JSParser()

  def ps(r: p.ParseResult[Any]): Unit = {
    println(r)
    r match {
      case p.Success(_, _) =>
      case p.NoSuccess(msg, rest) => fail("parsing failed: " + msg)
    }
  }


  def getSource(f: String) =
    io.Source.fromFile(f).getLines().map(
      l => if (l.contains("//")) l.take(l.indexOf("//")) else l
    ).map(
      l => if (l.startsWith("#!")) "" else l
    ).mkString("\n")

  def printProg(s: Statement, indent: Int = 0): Unit = {
    val in = "  " * indent
    s match {
      case Sequence(inner) => inner.reverse.map(printProg(_, indent))
      case FunDecl(v, args, body) =>
        println(in + s"FunDecl $v, $args:")
        printProg(body, indent + 1)
      case ConditionalStatement(a, b) =>
        println(in + s"if:")
        printProg(a, indent + 1)
        println(in + "else:")
        printProg(b, indent + 1)
      case LoopStatement(a) =>
        println(in + s"loop:")
        printProg(a, indent + 1)
      case _ => println(in + s.toString)
    }
  }


  def printEnv(env: Env): Unit = {
    println("### Env: ")
    println(env)

    val ret = NamedVariable("$return")
    val retObjs = env.lookup(ret)._1

    var printed: Set[Value] = Set()

    def printValue(v: Value): Unit = if (!(printed contains v)) {
      printed += v
      v match {
        case callret: MethodReturnValue =>
          printObj(callret)
          println("\t\"" + callret + "\" [ style=bold ]")
          callret.target.foreach(t => println("\t\"" + callret + "\" -> \"" + t + "\" [ label = \"call-target\", style=dotted  ];"))
          //          callret.thisObj.foreach(t => println("\t\"" + callret + "\" -> \"" + t + "\" [ label = \"call-this\", style=dotted  ];"))
          for ((args, idx) <- callret.args.zipWithIndex; arg <- args)
            println("\t\"" + callret + "\" -> \"" + arg + "\" [ label = \"call-arg" + (idx + 1) + "\", style=dotted  ];")
          (callret.target ++ /*callret.thisObj++*/ callret.args.flatten).foreach(printValue)
        case obj: Obj =>
          printObj(obj)
        case _ =>
      }
    }
    def printObj(obj: Obj): Unit = {
      //fields (writes)
      val fields = env.members.getOrElse(obj, Map())
      for ((field, fieldValues) <- fields;
           fieldValue <- fieldValues) {
        println("\t\"" + obj + "\" -> \"" + fieldValue + "\" [ label = \"" + field + "\", style=dashed  ];")
        printValue(fieldValue)
      }
      //reverse lookup (reads)
      for ((sourceObj, fields) <- env.members.mapValues(_.filter(_._2 contains obj).keySet).filter(_._2.nonEmpty);
           field <- fields) {
        //        println("\t\"" + sourceObj + "\" -> \"" + obj + "\" [ label = \"" + field + "\", style=dashed  ];")
        printValue(sourceObj)
      }
    }

    println("digraph G {")

    println("\t\"" + ret + "\" [ style=filled ]")
    for (retObj <- retObjs) {
      println("\t\"" + ret + "\" -> \"" + retObj + "\"")
      printValue(retObj)
    }


    println("}")

  }

}
