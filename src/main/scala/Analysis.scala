//package edu.cmu.cs.nodesec
//
//
///**
//  * Created by ckaestne on 11/24/16.
//  */
//class Analysis {
//
//  trait Value
//
//  object Unknown extends Value()
//
//  var defuse: Map[Id, VarDef] = Map()
//  var aliases: Map[Id, Set[Value]] = Map()
//
//  case class Result(msg: String)
//
//
//  def analyze(p: CompoundStmt): List[Result] = {
//
//
//    iterateAST(p, findUnknownFunctions)
//
//
//  }
//
//  def findUnknownFunctions(a: AST): List[Result] = a match {
//    case FunCall(id: Id, _) =>
//      if (aliases.getOrElse(id, Set(Unknown)) contains Unknown)
//        Result(s"unresolved function call to ${id.a}()") :: Nil
//      else Nil
//    case FunCall(expr, _) =>
//      Result(s"cannot resolve computed target of function call ($expr())") :: Nil
//    case _ => Nil
//  }
//
//
//  def iterateAST(a: Any, fun: (AST) => List[Result]): List[Result] = {
//    def it(a: Any): List[Result] = iterateAST(a, fun)
//
//    if (a.isInstanceOf[AST]) {
//      fun(a.asInstanceOf[AST]) ++ a.asInstanceOf[AST].productIterator.map(it).foldLeft[List[Result]](Nil)(_ ++ _)
//    }
//    else if (a.isInstanceOf[List[_]])
//      a.asInstanceOf[List[_]].map(it).foldLeft[List[Result]](Nil)(_ ++ _)
//    else if (a.isInstanceOf[Option[_]])
//      a.asInstanceOf[Option[_]].fold[List[Result]](Nil)(it)
//    else if (a.isInstanceOf[(_, _)])
//      it(a.asInstanceOf[(_, _)]._1) ++ it(a.asInstanceOf[(_, _)]._2)
//    else Nil
//  }
//
//
//}
