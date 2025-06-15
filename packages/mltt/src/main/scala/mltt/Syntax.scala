package mltt.syntax

import fastparse._, fastparse.ScalaWhitespace._

case class Let(isType: Boolean, name: String, ty: Option[Expr], init: Expr)
case class Prog(defs: List[Let])

sealed abstract class Expr
final case class Name(name: String) extends Expr
final case class Ext(name: String) extends Expr
final case class Todo() extends Expr
final case class UniE(level: Int) extends Expr
final case class Lam(name: String, ty: Expr, body: Expr, lvl: Boolean)
    extends Expr
final case class App(func: Expr, arg: Expr) extends Expr

type Type = Expr

object Parser {
  def parse(src: String): Prog = {
    fastparse.parse(src, Parser.root(_)) match {
      case Parsed.Success(prog, _) => prog
      case Parsed.Failure(_, index, extra) =>
        println(extra.trace().longAggregateMsg)
        println(src.slice(index, index + 40))
        throw new Exception("Parsing failed")
    }
  }

  def root[$: P] = P(("" ~ stmt).rep.map(_.toList).map(Prog.apply) ~ End)

  def stmt[$: P]: P[Let] = P(type0Def | typeDef | valDef)
  def type0Def[$: P] =
    P(kw("type") ~ id ~ ";").map(n => Let(true, n, None, Ext(n)))
  def typeDef[$: P] =
    P(kw("type") ~/ id ~ ("=" ~/ term) ~ ";")
      .map(Let(true, _, None, _))
  def valDef[$: P] =
    P(kw("val") ~/ id ~ (":" ~/ term).? ~ "=" ~/ term ~ ";")
      .map(Let(false, _, _, _))

  def term[$: P]: P[Expr] = P(uniTerm | lamTerm | appTerm | varTerm | todoTerm)
  def uniTerm[$: P] = P(kw("Type")).map(_ => UniE(0))
  def lamTerm[$: P] =
    P("(" ~/ id ~ ":" ~ term ~ ")" ~ "=>" ~ term)
      .map(Lam(_, _, _, false))
  def appTerm[$: P] =
    P(varTerm ~ ("(" ~/ term.rep(sep = ",") ~ ",".? ~ ")").rep)
      .map { case (func, args) => args.flatten.foldLeft(func)(App.apply) }
  def varTerm[$: P] = P(id).map(Name.apply)
  def todoTerm[$: P] = P("???").map(_ => Todo())

  def kw[$: P](s: String) = s ~~ !idCont
  def letter[$: P] = P(CharIn("a-z") | CharIn("A-Z"))
  def digit[$: P] = P(CharIn("0-9"))
  def id[$: P] = P((letter | "_") ~~ idCont.repX).!.filter(!keywords(_))
  def idCont[$: P] = P(letter | digit | "_")

  val keywords = Set("type", "var")
}
