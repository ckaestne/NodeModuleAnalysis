package edu.cmu.cs.nodesec.parser

import scala.util.parsing.combinator._


class JSParser extends RegexParsers {

  def Program: Parser[FunctionBody] = positioned(Element.* ^^ FunctionBody)

  def FunBody: Parser[FunctionBody] = "{"~> Program <~ "}"

  def Element: Parser[Stmt] =
    FunctionDeclaration |
      Statement

  def ParameterList: Parser[List[Id]] = repsep(Identifier, ",")

  def CompoundStatement: Parser[Stmt] = positioned("{" ~> rep(Statement) <~ "}" ^^ CompoundStmt)

  def Statement: Parser[Stmt] = positioned(
    ";" ^^^ EmptyStmt() |
      "if" ~> Condition ~ Statement ~ opt("else" ~> Statement) ^^ ifStmt |
      "while" ~> Condition ~ Statement ^^ whileStmt |
      "for" ~ "(" ~ ";" ~ Expression.? ~ ";" ~ Expression.? ~ ")" ~ Statement ^^ NotImplStmt |
      "for" ~ "(" ~ VariablesOrExpression ~ ";" ~ Expression.? ~ ";" ~ Expression.? ~ ")" ~ Statement ^^ NotImplStmt |
      "for" ~ "(" ~ VariablesOrExpression ~ "in" ~ Expression ~ ")" ~ Statement ^^ NotImplStmt |
      "break" ~ ";" ^^ NotImplStmt |
      "continue" ~ ";" ^^ NotImplStmt |
      "with" ~ "(" ~ Expression ~ ")" ~ Statement ^^ NotImplStmt |
      "return" ~> Expression.? <~ ";" ^^ ReturnStmt |
      CompoundStatement |
      VariablesOrExpression <~ ";")


  def Condition = "(" ~> Expression <~ ")"


  def VariablesOrExpression: Parser[Stmt] =
    "var" ~> repsep(Variable, ",") ^^ VarStmt |
      Expression ^^ ExpressionStmt


  def Variable: Parser[VarDef] = Identifier ~ opt("=" ~> AssignmentExpression) ^^ varDef

  def Expression: Parser[Expr] = positioned(AssignmentExpression ~ opt("," ~ Expression) ^^ binExpr)

  def AssignmentExpression: Parser[Expr] = ConditionalExpression ~ opt(AssignmentOperator ~ AssignmentExpression) ^^ assignExpr

  def ConditionalExpression: Parser[Expr] = OrExpression ~ opt(("?" ~> AssignmentExpression <~ ":") ~ AssignmentExpression) ^^ iteExpr

  def OrExpression: Parser[Expr] = AndExpression ~ opt("||" ~ OrExpression) ^^ binExpr

  def AndExpression: Parser[Expr] = BitwiseOrExpression ~ opt("&&" ~ AndExpression) ^^ binExpr

  def BitwiseOrExpression: Parser[Expr] = BitwiseXorExpression ~ opt("|" ~ BitwiseOrExpression) ^^ binExpr

  def BitwiseXorExpression: Parser[Expr] = BitwiseAndExpression ~ opt("^" ~ BitwiseXorExpression) ^^ binExpr

  def BitwiseAndExpression: Parser[Expr] = EqualityExpression ~ opt("&" ~ BitwiseAndExpression) ^^ binExpr

  def EqualityExpression: Parser[Expr] = RelationalExpression ~ opt(EqualityOperator ~ EqualityExpression) ^^ binExpr

  def RelationalExpression: Parser[Expr] = ShiftExpression ~ opt(RelationalationalOperator ~ RelationalExpression) ^^ binExpr

  def RelationalationalOperator = "<=" | ">=" | "<" | ">" | "instanceof" | "in"

  def ShiftExpression: Parser[Expr] = AdditiveExpression ~ opt(ShiftOperator ~ ShiftExpression) ^^ binExpr

  def ShiftOperator = ">>>" | ">>" | "<<"

  def AdditiveExpression: Parser[Expr] = MultiplicativeExpression ~ opt(("+" | "-") ~ AdditiveExpression) ^^ binExpr

  def MultiplicativeExpression: Parser[Expr] = UnaryExpression ~ opt(("*" | "%" | "/") ~ MultiplicativeExpression) ^^ binExpr

  def UnaryExpression: Parser[Expr] =
    ("delete" | "void" | "typeof") ~ UnaryExpression ^^ unaryExpr |
      ("-" | "+" | "!") ~ UnaryExpression ^^ unaryExpr |
      ("++" | "--") ~ MemberExpression ^^ unaryExpr |
      "new" ~> MemberExpression ~ opt(Args) ^^ newExpr |
      "delete" ~ MemberExpression ^^ unaryExpr |
      MemberExpression ~ opt("++" | "--") ^^ postExpr


  def Args = "(" ~ repsep(AssignmentExpression, ",") <~ ")"

  def MemberExpression: Parser[Expr] = positioned(
    FunctionExpression |
      PrimaryExpression ~ (rep(
        Args |
          "." ~ Identifier |
          "[" ~ Expression <~ "]"
      ) ^^ {
        _.reverse
      }) ^^ memberExpr)


  def FunctionDeclaration: Parser[FunDeclaration] =
    (("function" ~> Identifier <~ "(") ~ ParameterList <~ ")") ~ FunBody ^^ funDecl

  def FunctionExpression: Parser[Expr] =
    positioned((("function" ~> opt(Identifier) <~ "(") ~ ParameterList <~ ")") ~ FunBody ^^ funExpr)

  def PrimaryExpression: Parser[Expr] =
    "(" ~> Expression <~ ")" |
      Identifier |
      ObjectLiteral |
      ArrayLiteral |
      IntegerLiteral |
      StringLiteral |
      "false" ^^ ConstExpr |
      "true" ^^ ConstExpr |
      "null" ^^ ConstExpr |
      "this" ^^ ConstExpr

  def ArrayLiteral: Parser[Expr] = "[" ~ repsep(rep(",") ~ AssignmentExpression, ",") ~ "]" ^^ NotImplExpr

  def ObjectLiteral: Parser[ObjExpr] = positioned("{" ~> repsep(PropertyAssignment, ",") <~ opt(",") ~ "}" ^^ ObjExpr)

  def PropertyAssignment: Parser[(String,Expr)] =
    (PropertyName <~ ":") ~ AssignmentExpression ^^ {case a~b=>(a,b)}
//  |
//      "get" ~ PropertyName ~ "(" ~ ")" ~ CompoundStatement |
//      "set" ~ PropertyName ~ "(" ~ Identifier ~ ")" ~ CompoundStatement

  def PropertyName: Parser[String] =
    Identifier ^^ {_.a} |
      StringLiteral ^^ {_.a} |
      IntegerLiteral ^^ {_.a}


  def Identifier: Parser[Id] = positioned("""[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ Id)

  def IntegerLiteral: Parser[Lit] = positioned("""[0-9]+""".r ^^ Lit )//TODO floating point

  def EqualityOperator = "===" | "!==" | "==" | "!="

  def AssignmentOperator =
    "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|=" | "="

  def StringLiteral = //TODO fix
    positioned("\"[^\"]*\"".r ^^ Lit |
      "'[^']*'".r ^^ Lit   |
      "/[^/]*/g?".r ^^ Lit)


  def assignExpr(a: Expr ~ Option[String ~ Expr]) = a match {
    case x ~ Some(o ~ y) => AssignExpr(x, o, y)
    case x ~ None => x
  }

  def binExpr(a: Expr ~ Option[String ~ Expr]) = a match {
    case x ~ Some(o ~ y) => BinExpr(x, o, y)
    case x ~ None => x
  }

  def iteExpr(a: Expr ~ Option[Expr ~ Expr]) = a match {
    case x ~ Some(t ~ e) => ITEExpr(x, t, e)
    case x ~ None => x
  }

  def postExpr(a: Expr ~ Option[String]) = a match {
    case x ~ Some(y) => PostExpr(x, y)
    case x ~ None => x
  }

  def varDef(a: Id ~ Option[Expr]) = VarDef(a._1, a._2)

  def unaryExpr(a: String ~ Expr) = UnaryExpr(a._1, a._2)

  def newExpr(a: Expr ~ Option[String ~ List[Expr]]) = NewExpr(a._1, a._2.map(_._2).getOrElse(Nil))

  def whileStmt(a: Expr ~ Stmt) = WhileStmt(a._1, a._2)

  def ifStmt(a: Expr ~ Stmt ~ Option[Stmt]) = IfStmt(a._1._1, a._1._2, a._2)

  def funExpr(a: (Option[Id] ~ List[Id]) ~ FunctionBody) = FunExpr(a._1._1, a._1._2, a._2)
  def funDecl(a: (Id ~ List[Id]) ~ FunctionBody) = FunDeclaration(a._1._1, a._1._2, a._2)

  def memberExpr(x: Expr ~ List[String ~ Any]): Expr = x match {
    case a ~ Nil => a
    case a ~ (h :: r) =>
      val target = memberExpr(new ~(a, r))
      h match {
        case "." ~ (e:Id) => FieldAcc(target, e).setPos(e.pos)
        case "[" ~ e => NotImplExpr(h)
        case "(" ~ e => FunCall(target, e.asInstanceOf[List[Expr]]).setPos(target.pos)
      }
  }

}

