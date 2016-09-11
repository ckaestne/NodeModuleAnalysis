package edu.cmu.cs.nodesec

import scala.util.parsing.combinator._


class JSParser extends RegexParsers {

  def Program: Parser[CompoundStmt] = Element.* ^^ CompoundStmt


  def Element: Parser[Stmt] =
    FunctionExpression ^^ ExpressionStmt |
      Statement

  def ParameterList: Parser[List[Id]] = repsep(Identifier, ",")

  def CompoundStatement: Parser[Stmt] = "{" ~> rep(Statement) <~ "}" ^^ CompoundStmt

  def Statement: Parser[Stmt] =
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
      CompoundStatement ^^ NotImplStmt |
      VariablesOrExpression <~ ";"


  def Condition = "(" ~> Expression <~ ")"


  def VariablesOrExpression: Parser[Stmt] =
    "var" ~> repsep(Variable, ",") ^^ VarStmt |
      Expression ^^ ExpressionStmt


  def Variable: Parser[VarDef] = Identifier ~ opt("=" ~> AssignmentExpression) ^^ varDef

  def Expression: Parser[Expr] = AssignmentExpression ~ opt("," ~ Expression) ^^ binExpr

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

  def MemberExpression: Parser[Expr] =
    FunctionExpression |
      PrimaryExpression ~ (rep(
        Args |
          "." ~ Identifier |
          "[" ~ Expression <~ "]"
      ) ^^ {
        _.reverse
      }) ^^ memberExpr

  def FunctionExpression: Parser[Expr] = (("function" ~> opt(Identifier) <~ "(") ~ ParameterList <~ ")") ~ CompoundStatement ^^ funExpr

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

  def ObjectLiteral: Parser[Expr] = "{" ~ repsep(PropertyAssignment, ",") ~ opt(",") ~ "}" ^^ NotImplExpr

  def PropertyAssignment: Parser[Any] =
    PropertyName ~ ":" ~ AssignmentExpression |
      "get" ~ PropertyName ~ "(" ~ ")" ~ CompoundStatement |
      "set" ~ PropertyName ~ "(" ~ Identifier ~ ")" ~ CompoundStatement

  def PropertyName =
    Identifier |
      StringLiteral |
      IntegerLiteral


  def Identifier: Parser[Id] = """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ Id

  def IntegerLiteral: Parser[Lit] = """[0-9]+""".r ^^ Lit //TODO floating point

  def EqualityOperator = "===" | "!==" | "==" | "!="

  def AssignmentOperator =
    "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|=" | "="

  def StringLiteral = //TODO fix
    "\"[^\"]*\"".r ^^ Lit |
      "'[^']*'".r ^^ Lit


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

  def funExpr(a: (Option[Id] ~ List[Id]) ~ Stmt) = FunExpr(a._1._1, a._1._2, a._2)

  def memberExpr(x: Expr ~ List[String ~ Any]): Expr = x match {
    case a ~ Nil => a
    case a ~ (h :: r) =>
      val target = memberExpr(new ~(a, r))
      h match {
        case "." ~ e => FieldAcc(target, e.asInstanceOf[Id])
        case "[" ~ e => NotImplExpr(h)
        case "(" ~ e => FunCall(target, e.asInstanceOf[List[Expr]])
      }
  }

}

