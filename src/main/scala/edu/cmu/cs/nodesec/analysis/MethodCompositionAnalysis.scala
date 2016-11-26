package edu.cmu.cs.nodesec.analysis

import edu.cmu.cs.nodesec.datalog.{Datalog, _}

import scala.util.parsing.input.{NoPosition, Position}

/**
  * Created by ckaestne on 11/25/16.
  */
object MethodCompositionAnalysis {

  case class PolicyViolation(msg: String, pos: Position) {
    def render: String = {
      if (pos == NoPosition) msg
      else msg + " in line " + pos.line + "\n" + pos.longString
    }
  }

  trait Policy {
    def apply(datalog: Datalog, mainFun: FunDecl, methodSummaries: Set[(FunDecl, Env)]): Seq[PolicyViolation]

    def +(that: Policy): Policy = new ComposedPolicy(this, that)
  }


  class ComposedPolicy(p1: Policy, p2: Policy) extends Policy {
    def apply(d: Datalog, f: FunDecl, s: Set[(FunDecl, Env)]) = p1(d, f, s) ++ p2(d, f, s)
  }

  class NoCallToRequire extends Policy {


    def apply(d: Datalog, f: FunDecl, methodSummaries: Set[(FunDecl, Env)]) = {
      d.rule(Expr("callToRequire", "X"), /*:-*/ Expr("invoke", "A", "X"), Expr("pt", "A", f.uniqueId+ "param-require"))
      val result = d.query("callToRequire", "X")

      result.map(
        r => PolicyViolation("Potential call to 'require' found", getPosition(r("X"), methodSummaries))
      )
    }

    private def getPosition(retObjString: String, methodSummaries: Set[(FunDecl, Env)]): Position = {
      for ((fun, env) <- methodSummaries;
           if retObjString startsWith fun.uniqueId;
           (call, retVals) <- env.calls;
           retVal <- retVals;
           if (fun.uniqueId + retVal) == retObjString
      ) return call.pos
      return NoPosition
    }
  }

  lazy val noCallToRequire = new NoCallToRequire
}

class MethodCompositionAnalysis {

  import AnalysisHelper._
  import MethodCompositionAnalysis._

  def collectFunDecls(s: Statement): Set[FunDecl] = s match {
    case f: FunDecl => collectFunDecls(f.body) + f
    case Sequence(inner) => inner.foldLeft(Set[FunDecl]())(_ ++ collectFunDecls(_))
    case LoopStatement(inner) => collectFunDecls(inner)
    case ConditionalStatement(a, b) => collectFunDecls(a) ++ collectFunDecls(b)
    case _ => Set()
  }


  def analyzeScript(p: Statement, policy: Policy): Seq[PolicyViolation] = {
    val mainFun = AnalysisHelper.wrapScript(p)
    val funDecls = collectFunDecls(mainFun)

    val summaries = for (funDecl <- funDecls)
      yield (funDecl, new IntraMethodAnalysis().analyze(funDecl))

    val result = compose(summaries)


    policy(result, mainFun, summaries)
  }


  /**
    * this method composes the method summaries produced by `IntraMethodAnalysis`
    *
    * context insensitive for now (context sensitivity through copying requires cycle detection
    * for calls among modules first)
    */
  def compose(methodSummaries: Set[(FunDecl, Env)]): Datalog = {

    //    var inclusionEdges: Set[(Obj, Obj)] = Set()

    val datalog = new Datalog()

    datalog.rule(Expr("pt", "A", "C"), /*:-*/ Expr("pt", "A", "B"), Expr("pt", "B", "C"))
    datalog.rule(Expr("pt", "R", "O"), /*:-*/ Expr("invoke", "T", "R"), Expr("functionptr", "T", "F"), Expr("return", "F", "O"))
    datalog.rule(Expr("pt", "R", "O"), /*:-*/ Expr("invoke", "T", "R"), Expr("pt", "T", "Q"), Expr("functionptr", "Q", "F"), Expr("return", "F", "O"))
    datalog.rule(Expr("pt", "A", "B"), /*:-*/ Expr("actual", "T", "Z", "B"), Expr("functionptr", "T", "F"), Expr("formal", "F", "Z", "A"))
    datalog.rule(Expr("pt", "A", "B"), /*:-*/ Expr("actual", "T", "Z", "B"), Expr("pt", "T", "Q"), Expr("functionptr", "Q", "F"), Expr("formal", "F", "Z", "A"))
    datalog.rule(Expr("pt", "A", "B"), /*:-*/ Expr("member", "X", "F", "A"), Expr("member", "X", "F", "B"))
    datalog.rule(Expr("pt", "A", "B"), /*:-*/ Expr("member", "X", "F", "A"), Expr("pt", "X", "Y"), Expr("member", "Y", "F", "B"))

    println(datalog.ruleStr + "%%%")

    for ((method, summary) <- methodSummaries) {
      val facts = summaryToDatalog(method, summary)
      facts.foreach(datalog.load)
      facts.map(println)
      println("%")
    }


    datalog
  }

  def summaryToDatalog(fun: FunDecl, env: Env): List[DRelation] = {
    var result: List[DRelation] = Nil

    for ((arg, idx) <- fun.args.zipWithIndex;
         obj <- env.lookup(arg)._1)
      result ::= DFormal(fun, idx, obj)

    for (retvar <- Set(returnVariable); obj <- env.lookup(retvar)._1)
      result ::= DReturn(fun, obj)

    for ((obj, children) <- env.members;
         (field, fieldvalues) <- children;
         fieldvalue <- fieldvalues)
      result ::= DMember(fun, obj, field, fieldvalue)

    for ((call, retVals) <- env.calls;
         retVal <- retVals;
         target <- retVal.target) {
      result ::= DInvoke(fun, target, retVal)
      for ((argSet, idx) <- retVal.args.zipWithIndex; arg <- argSet)
        result ::= DActual(fun, target, idx, arg)
    }

    for ((obj, targetFun) <- env.functionPtrs)
      result ::= DFunctionPtr(fun, obj, targetFun)

    result
  }


}
