package edu.cmu.cs.nodesec.parser

import java.io.File

import jawn.{Parser, SimpleFacade}

import scala.util.parsing.input.{NoPosition, Position, Positional}

/**
  * parser, parsing the json output of esprima
  * (means that we have to only translate the esprima ast into our internals)
  */
class JSASTParser {


  def parse(json_ast: String) = {
    toProgram(Parser.parseFromString(json_ast)(JsStructure).get)
  }

  def parse(json_ast_file: File) = {
    toProgram(Parser.parseFromFile(json_ast_file)(JsStructure).get)
  }


  private trait JsValue

  private object JsNull extends JsValue

  private object JsFalse extends JsValue

  private object JsTrue extends JsValue

  private case class JsNumber(v: String) extends JsValue

  private case class JsString(v: String) extends JsValue

  private case class JsArray(vs: List[JsValue]) extends JsValue

  private case class JsObject(vs: Map[String, JsValue]) extends JsValue

  private case class ASTNode(t: String, vs: Map[String, JsValue], loc: Position) extends JsValue

  private object JsStructure extends SimpleFacade[JsValue] {
    def jnull() = JsNull

    def jfalse() = JsFalse

    def jtrue() = JsTrue

    def jnum(s: String) = JsNumber(s)

    def jint(s: String) = JsNumber(s)

    def jstring(s: String) = JsString(s)

    def jarray(vs: List[JsValue]) = JsArray(vs)

    private class APosition(val line: Int, val column: Int) extends Position {
      override protected def lineContents: String = ""
    }

    private def getLoc(vs: Map[String, JsValue]) = {
      val startLoc = child(child(Some(vs), "loc"), "start")
      if (startLoc.isDefined) {
        new APosition(int(startLoc.get("line")), int(startLoc.get("column")))
      } else NoPosition
    }

    private def int(v: JsValue) = v match {
      case JsNumber(s) => s.toInt
    }

    private def child(m: Option[Map[String, JsValue]], f: String): Option[Map[String, JsValue]] = m.flatMap(m =>
      m.get(f) match {
        case Some(JsObject(m)) => Some(m)
        case _ => None
      }
    )


    def jobject(vs: Map[String, JsValue]) = {
      val t = vs.get("type")
      t match {
        case Some(JsString(t)) => ASTNode(t, vs - "type" - "loc", getLoc(vs))
        case _ => JsObject(vs)
      }
    }
  }


  private def m_array(b: Map[String, JsValue], s: String): List[JsValue] = b.get(s).get match {
    case JsArray(v) => v
  }

  private def toProgram(json: JsValue): FunctionBody = positioned(json, json match {
    case ASTNode("Program", b, _) => FunctionBody(m_array(b, "body").map(toStmt))
  })

  private def toId(json: JsValue): Id = positioned(json, json match {
    case ASTNode("Identifier", b, _) => Id(toString(b("name")))
  })

  private def toString(json: JsValue): String = json match {
    case JsString(x) => x
  }

  private def toCatchClause(json: JsValue): (Expr, Stmt) = json match {
    case ASTNode("CatchClause", b, _) => (toExpr(b("param")), toStmt(b("body")))
  }

  private def toBool(json: JsValue): Boolean = json match {
    case JsFalse => false
    case JsTrue => true
  }


  def opt[T](fun: (JsValue) => T, json: JsValue): Option[T] = json match {
    case JsNull => None
    case _ => Some(fun(json))
  }


  private def positioned[T <: Positional](json: JsValue, ast: T): T = json match {
    case ASTNode(_, _, pos) => ast.setPos(pos)
    case _ => ast
  }

  private def toStmt(json: JsValue): Stmt = positioned(json, json match {
    case ASTNode("EmptyStatement", _, _) => EmptyStmt()
    case ASTNode("BlockStatement", b, _) => CompoundStmt(m_array(b, "body").map(toStmt))
    case ASTNode("ExpressionStatement", b, _) => ExpressionStmt(toExpr(b("expression")))
    case ASTNode("IfStatement", b, _) => IfStmt(toExpr(b("test")), toStmt(b("consequent")), opt(toStmt, b("alternate")))
    case ASTNode("LabeledStatement", b, _) => ???
    case ASTNode("BreakStatement", b, _) => BreakStmt(opt(toId, b("label")))
    case ASTNode("ContinueStatement", b, _) => ContinueStmt(opt(toId, b("label")))
    case ASTNode("WithStatement", b, _) => ???
    case ASTNode("SwitchStatement", b, _) => ???
    case ASTNode("ThrowStatement", b, _) => ThrowStmt(toExpr(b("argument")))
    case ASTNode("TryStatement", b, _) => TryStmt(toStmt(b("block")), opt(toCatchClause, b("handler")), opt(toStmt, b("finalizer")))
    case ASTNode("ReturnStatement", b, _) => ReturnStmt(opt(toExpr, b("argument")))
    case ASTNode("WhileStatement", b, _) => WhileStmt(toExpr(b("test")), toStmt(b("body")))
    case ASTNode("DoWhileStatement", b, _) => DoWhileStmt(toStmt(b("body")), toExpr(b("test")))
    case ASTNode("ForStatement", b, _) =>
      val (decl, init) = toForInit(b("init"))
      ForStmt(decl, init, opt(toExpr, b("test")), opt(toExpr, b("update")), toStmt(b("body")))
    case ASTNode("ForInStatement", b, _) =>
      val (decl, init) = toForInit(b("left"))
      ForInStmt(decl, init.get, toExpr(b("right")), toStmt(b("body")), toBool(b("each")))
    case ASTNode("ForOfStatement", b, _) => ???
    case ASTNode("LetStatement", b, _) => ???
    case ASTNode("DebuggerStatement", b, _) => ???
    case ASTNode("FunctionDeclaration", b, _) => FunDeclaration(
      toId(b("id")),
      m_array(b, "params").map(toId), //TODO pattern?
      FunctionBody(List(toStmt(b("body"))))
      //  defaults: [ Expression ];
      //  rest: Identifier | null;
      //  body: BlockStatement | Expression;
      //  generator: boolean;
      //  expression: boolean;
    )
    case ASTNode("VariableDeclaration", b, _) => VarStmt(m_array(b, "declarations").map(toVarDef))
  })

  private def toForInit(json: JsValue): (Option[VarDef], Option[Expr]) =
    json match {
      case ASTNode("VariableDeclaration", b, _) =>
        val d = m_array(b, "declarations").map(toVarDef).head
        if (d.init.isDefined)
          (Some(VarDef(d.name, None)), Some(AssignExpr(d.name, "=", d.init.get)))
        else
          (Some(VarDef(d.name, None)), Some(d.name))
      case e@ASTNode("Expression", b, _) => (None, Some(toExpr(e)))
      case JsNull => (None, None)
    }

  private def toVarDef(json: JsValue): VarDef = json match {
    case ASTNode("VariableDeclarator", b, _) => VarDef(toId(b("id")), opt(toExpr, b("init")))
  }


  private def toProperty(json: JsValue): (String, Expr) = json match {
    case ASTNode("Property", b, _) =>
      val key = b("key") match {
        case ASTNode("Identifier", b, _) => toString(b("name"))
        case ASTNode("Literal", b, _) => toString(b("value"))
      }
      //kind?
      (key, toExpr(b("value")))
  }

  private def toLit(json: JsValue): Lit = positioned(json, json match {
    case JsFalse => Lit("false")
    case JsTrue => Lit("true")
    case JsNumber(v) => Lit(v)
    case JsString(v) => Lit(v)
    case JsNull => Lit("null")
  })

  private def toExpr(json: JsValue): Expr = positioned(json, json match {
    case ASTNode("ThisExpression", b, _) => Id("this")
    case ASTNode("ArrayExpression", b, _) => ArrayExpr(m_array(b, "elements").map(toExpr))
    case ASTNode("ObjectExpression", b, _) => ObjExpr(m_array(b, "properties").map(toProperty))
    case ASTNode("FunctionExpression", b, _) =>
      FunExpr(
        opt(toId, b("id")),
        m_array(b, "params").map(toId), //TODO pattern?
        FunctionBody(List(toStmt(b("body"))))
        //  defaults: [ Expression ];
        //  rest: Identifier | null;
        //  body: BlockStatement | Expression;
        //  generator: boolean;
        //  expression: boolean;
      )
    case ASTNode("ArrowExpression", b, _) =>
      FunExpr(
        None,
        m_array(b, "params").map(toId),
        FunctionBody(List(toStmt(b("body"))))
        //TODO others
      )
    case ASTNode("SequenceExpression", b, _) => ???
    case ASTNode(x, b, _) if x == "UpdateExpression" || x == "UnaryExpression" =>
      if (toBool(b("prefix")))
        UnaryExpr(toString(b("operator")), toExpr(b("argument")))
      else
        PostExpr(toExpr(b("argument")), toString(b("operator")))
    case ASTNode("BinaryExpression", b, _) =>
      BinExpr(toExpr(b("left")), toString(b("operator")), toExpr(b("right")))
    case ASTNode("LogicalExpression", b, _) =>
      BinExpr(toExpr(b("left")), toString(b("operator")), toExpr(b("right")))
    case ASTNode("AssignmentExpression", b, _) =>
      AssignExpr(toExpr(b("left")), toString(b("operator")), toExpr(b("right")))
    case ASTNode("ConditionalExpression", b, _) =>
      ITEExpr(toExpr(b("test")), toExpr(b("alternate")), toExpr(b("consequent")))
    case ASTNode("NewExpression", b, _) =>
      NewExpr(toExpr(b("callee")), m_array(b, "arguments").map(toExpr))
    case ASTNode("CallExpression", b, _) =>
      FunCall(toExpr(b("callee")), m_array(b, "arguments").map(toExpr))
    case ASTNode("MemberExpression", b, _) =>
      if (toBool(b("computed")))
        DynFieldAcc(toExpr(b("object")), toExpr(b("property")))
      else
        FieldAcc(toExpr(b("object")), toId(b("property")))
    case ASTNode("YieldExpression", b, _) => ???
    case ASTNode("ComprehensionExpression", b, _) => ???
    case ASTNode("GeneratorExpression", b, _) => ???
    case ASTNode("GraphExpression", b, _) => ???
    case ASTNode("GraphIndexExpression", b, _) => ???
    case ASTNode("LetExpression", b, _) => ???
    case ASTNode("Literal", b, _) => toLit(b("raw"))
    case ASTNode("Identifier", b, _) => Id(toString(b("name")))
  })

  //  private m_array()


}

