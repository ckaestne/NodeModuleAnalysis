package edu.cmu.cs.nodesec

import scala.util.parsing.combinator._


class JSParser extends RegexParsers {

  def Program: Parser[Any] = Element *

  def Element: Parser[Any] =
    FunctionExpression |
      Statement

  def ParameterList: Parser[Any] = repsep(Identifier, ",")

  def CompoundStatement: Parser[Any] = "{" ~ rep(Statement) ~ "}"

  def Statement: Parser[Any] =
    ";" |
      "if" ~ Condition ~ Statement |
      "if" ~ Condition ~ Statement ~ "else" ~ Statement |
      "while" ~ Condition ~ Statement |
      "for" ~ "(" ~ ";" ~ Expression.? ~ ";" ~ Expression.? ~ ")" ~ Statement |
      "for" ~ "(" ~ VariablesOrExpression ~ ";" ~ Expression.? ~ ";" ~ Expression.? ~ ")" ~ Statement |
      "for" ~ "(" ~ VariablesOrExpression ~ "in" ~ Expression ~ ")" ~ Statement |
      "break" ~ ";" |
      "continue" ~ ";" |
      "with" ~ "(" ~ Expression ~ ")" ~ Statement |
      "return" ~ Expression.? ~ ";" |
      CompoundStatement |
      VariablesOrExpression ~ ";" |
      Expression ~ ";"


  def Condition: Parser[Any] = "(" ~ Expression ~ ")"


  def VariablesOrExpression: Parser[Any] = "var" ~ repsep(Variable, ",") | Expression


  def Variable: Parser[Any] = Identifier ~ opt("=" ~ AssignmentExpression)

  def Expression: Parser[Any] = AssignmentExpression ~ opt("," ~ Expression)

  def AssignmentExpression: Parser[Any] = ConditionalExpression ~ opt(AssignmentOperator ~ AssignmentExpression)

  def ConditionalExpression: Parser[Any] = OrExpression ~ opt("?" ~ AssignmentExpression ~ ":" ~ AssignmentExpression)

  def OrExpression: Parser[Any] = AndExpression ~ opt("||" ~ OrExpression)

  def AndExpression: Parser[Any] = BitwiseOrExpression ~ opt("&&" ~ AndExpression)

  def BitwiseOrExpression: Parser[Any] = BitwiseXorExpression ~ opt("|" ~ BitwiseOrExpression)

  def BitwiseXorExpression: Parser[Any] = BitwiseAndExpression ~ opt("^" ~ BitwiseXorExpression)

  def BitwiseAndExpression: Parser[Any] = EqualityExpression ~ opt("&" ~ BitwiseAndExpression)

  def EqualityExpression: Parser[Any] = RelationalExpression ~ opt(EqualityOperator ~ EqualityExpression)

  def RelationalExpression: Parser[Any] = ShiftExpression ~ opt(RelationalationalOperator ~ RelationalExpression)

  def RelationalationalOperator = "<=" | ">=" | "<" | ">" | "instanceof" | "in"

  def ShiftExpression: Parser[Any] = AdditiveExpression ~ opt(ShiftOperator ~ ShiftExpression)

  def ShiftOperator = ">>>" | ">>" | "<<"

  def AdditiveExpression: Parser[Any] = MultiplicativeExpression ~ opt(("+" | "-") ~ AdditiveExpression)

  def MultiplicativeExpression: Parser[Any] = UnaryExpression ~ opt(("*" | "%" | "/") ~ MultiplicativeExpression)

  def UnaryExpression: Parser[Any] =
    ("delete" | "void" | "typeof") ~ UnaryExpression |
      ("-" | "+" | "!") ~ UnaryExpression |
      ("++" | "--") ~ MemberExpression |
      "new" ~ MemberExpression |
      "delete" ~ MemberExpression |
      MemberExpression ~ opt("++" | "--")


  def Args = "(" ~ repsep(AssignmentExpression, ",") ~ ")"

  def MemberExpression: Parser[Any] =
    FunctionExpression |
      PrimaryExpression ~ rep(
        Args |
          "." ~ MemberExpression |
          "[" ~ Expression ~ "]"
      )

  def FunctionExpression: Parser[Any] = "function" ~ opt(Identifier) ~ "(" ~ ParameterList ~ ")" ~ CompoundStatement

  def PrimaryExpression: Parser[Any] =
    "(" ~ Expression ~ ")" |
      Identifier |
      ObjectLiteral |
      ArrayLiteral |
      IntegerLiteral |
      StringLiteral |
      "false" |
      "true" |
      "null" |
      "this"

  def ArrayLiteral: Parser[Any] = "[" ~ repsep(rep(",") ~ AssignmentExpression, ",") ~ "]"

  def ObjectLiteral: Parser[Any] = "{" ~ repsep(PropertyAssignment, ",") ~ opt(",") ~ "}"

  def PropertyAssignment: Parser[Any] =
    PropertyName ~ ":" ~ AssignmentExpression |
      "get" ~ PropertyName ~ "(" ~ ")" ~ CompoundStatement |
      "set" ~ PropertyName ~ "(" ~ Identifier ~ ")" ~ CompoundStatement

  def PropertyName =
    Identifier |
      StringLiteral |
      IntegerLiteral


  def Identifier: Parser[Any] = """[a-zA-Z_][a-zA-Z0-9_]*""".r

  def IntegerLiteral: Parser[Any] = """[0-9]+""".r //TODO floating point

  def EqualityOperator: Parser[Any] = "===" | "!==" | "==" | "!="

  def AssignmentOperator: Parser[Any] =
    "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | ">>>=" | "&=" | "^=" | "|=" | "="

  def StringLiteral = //TODO fix
    "\"[^\"]*\"".r |
      "'[^']*'".r

}